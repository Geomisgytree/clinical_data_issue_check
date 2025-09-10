
##### Common issue check file #####
## ---- Issue included:
## ID: 1, Function: check_missing_key_vars
## ID: 2, Function: check_duplicate_visit_date
## ID: 3, Function: check_visit_after_ED
## ID: 4, Function: check_visit_overlap


library(dplyr)
library(rlang)


issue_envelope <- function(df_summary, df_raw, idx, label){
  list(summary = df_summary,
       raw     = df_raw)
}

raw_process <- function(raw, issue){
  raw <- raw %>% 
    mutate(Issue_type = "Automatic", 
           Issue_noted_by_Lilly_Stats = issue,
           PPD_Comment_or_resolution = "",
           Status = "New")
  
  raw[] <- lapply(raw, function(x) {
    if (inherits(x, "POSIXct") | inherits(x, "POSIXlt")) {
      as.character(x) 
    } else if (inherits(x, "Date")) {
      as.character(x) 
    } else {
      x
    }
  })
  
  return(raw)
}

sum_process <- function(sum, issue, ds_name, date_col = NULL){
  sum <- sum %>%
    group_by(SITEID, SUBJECT_ID) %>% 
    summarise(
      DATASET = ds_name,
      ISSUE = issue,
      FIRST_DETECTED_DATE = Sys.Date(), 
      LAST_CHECKED_DATE   = Sys.Date(),
      STATUS = "New", 
      .groups = "drop"
    ) %>% 
    select(DATASET, SUBJECT_ID, SITEID, ISSUE, FIRST_DETECTED_DATE, LAST_CHECKED_DATE, STATUS)
  return(sum)
}


##### 3.1 VISITNUM is NA #####

check_missing_key_vars <- function(datasets_pool, wb, 
                                   target_vars = NULL,
                                   dataset_no = NULL,
                                   visit_info_df = NULL,
                                   output_tab = NULL){
  
  datasets <- setdiff(names(datasets_pool), dataset_no) ## exclude un-used datasets
  
  if (length(datasets) == 0) {
    warning("Missing key var check skipped: no datasets used")
    return(invisible(NULL))
  }
  
  for (ds in datasets) {
    df <- datasets_pool[[ds]]
    
    common <- intersect(target_vars, names(df))
    if (!length(common)) {
      message("Dataset ", ds, " skipped: no common visit key variables found.")
      next
    }
    
    ## Convert empty string to NA for character columns
    char_vars <- intersect(common, names(df)[sapply(df, is.character)])
    df <- df %>% mutate(across(all_of(char_vars), ~na_if(.x, "")))
    
    ## Filter rows where any of the key vars are missing
    flagged <- df %>% filter(if_any(all_of(common), is.na))
    
    if (nrow(flagged) == 0) {
      next
    }
    
    flagged <- flagged %>%
      mutate(
        Issue_type = "Automatic",
        Issue_noted_by_Lilly_Stats = "Missing values in VISIT/VISITNUM/EVENT/EVENTDEF/EVENTEID",
        PPD_Comment_or_resolution = "",
        Status = "New"
      )
    
    # Convert all columns to character to avoid Excel format issues
    flagged[] <- lapply(flagged, as.character)
    
    ## Check if sheet exists
    if (!(ds %in% names(wb))) {
      addWorksheet(wb, ds)
      start_row <- 1
      write_header <- TRUE
    } else {
      # Try reading existing data to find last row
      existing_df <- tryCatch(readWorkbook(wb, sheet = ds), error = function(e) NULL)
      if (is.null(existing_df)) {
        start_row <- 1
        write_header <- TRUE
      } else {
        start_row <- nrow(existing_df) + 2
        write_header <- FALSE
      }
    }
    
    ## Write data
    writeData(wb, sheet = ds, x = flagged, startRow = start_row, colNames = write_header)
  }
  
  invisible(NULL)
}


##### 3.2 Duplicated visits with the same date #####

check_duplicate_visit_date <- function(datasets_pool, wb, 
                                       target_vars = NULL,
                                       dataset_no = NULL,
                                       visit_info_df = NULL,
                                       output_tab = NULL,
                                       custom_code_list = NULL) {
  
  datasets <- setdiff(names(datasets_pool), dataset_no)
  
  for (ds_name in datasets) {
    df <- datasets_pool[[ds_name]]
    if (is.null(df) || nrow(df) == 0) {
      message("Dataset empty or NULL: ", ds_name)
      next
    }
    
    info_row <- visit_info_df %>% filter(dataset == ds_name)
    
    # Skip if no row found in visit_info_df
    if (nrow(info_row) == 0) {
      message("Skip: ", ds_name, " - not found in visit_info_df.")
      next
    }
    
    visit_date_col <- info_row$visit_date_col
    visit_label_col <- info_row$visit_label_col
    
    if (is.na(visit_date_col) | !(visit_date_col %in% names(df))) {
      warning("Skip: ", ds_name, " - visit_date_col not found.")
      next
    }
    
    ## Basic filtering - preprocessing
    df <- df %>% filter(!is.na(.data[[visit_date_col]]))
    if (!is.na(visit_label_col) & visit_label_col %in% names(df)) {
      df <- df %>% filter(!grepl("unscheduled", .data[[visit_label_col]], ignore.case = TRUE))
    }
    
    ## Customized code
    if (!is.null(custom_code_list) && ds_name %in% names(custom_code_list)) {
      dups <- custom_code_list[[ds_name]](df, visit_date_col)
    } else {
      ## If no customized code - default group_by
      dups <- df %>%
        group_by(SUBJECT_ID, .data[[visit_date_col]]) %>%
        filter(n() > 1) %>%
        ungroup()
    }
    
    if (nrow(dups) == 0) {next}
    
    dups <- raw_process(
      dups %>% arrange(SUBJECT_ID, .data[[visit_date_col]], .data[[visit_label_col]]),
      issue = "Duplicate visits with the same date"
    )
    
    if (!(ds_name %in% names(wb))) {
      addWorksheet(wb, ds_name)
      start_row <- 1
      write_header <- TRUE
    } else {
      existing_df <- tryCatch(readWorkbook(wb, sheet = ds_name), error = function(e) NULL)
      if (is.null(existing_df)) {
        start_row <- 1
        write_header <- TRUE
      } else {
        start_row <- nrow(existing_df) + 2
        write_header <- FALSE
      }
    }
    writeData(wb, sheet = ds_name, x = dups, startRow = start_row, colNames = write_header)
  }
  
}




##### 3.3 Visits after ED #####
check_visit_after_ED <- function(datasets_pool, wb,
                                 dataset_no = NULL,
                                 visit_info_df = NULL,
                                 output_tab = NULL,
                                 other_datasets = NULL) {
  
  ed_ds <- datasets_pool[["ds6001"]]
  if (is.null(ed_ds) || !("DSSTDAT" %in% names(ed_ds))) {
    warning("ED dataset missing or DSSTDAT not found.")
    return(NULL)
  }
  
  ## Find ED dates
  ed_dates <- ed_ds %>%
    filter(FORMEID %in% c("DS6001_LV6", "DS6001_LV5") & DSSTDAT != "") %>%
    mutate(ED_DATE = as.Date(DSSTDAT)) %>%
    select(SUBJECT_ID, ED_DATE)
  
  datasets <- other_datasets
  
  for (ds_name in datasets) {
    
    df <- datasets_pool[[ds_name]]
    if (is.null(df) || nrow(df) == 0) {
      message("Dataset empty or NULL: ", ds_name)
      next
    }
    
    visit_date_col <- visit_info_df %>% filter(dataset == ds_name) %>% pull(visit_date_col)
    if (is.na(visit_date_col) | !(visit_date_col %in% names(df))) {
      warning("Skip: ", ds_name, " - visit_date_col not found.")
      next
    }
    
    df <- df %>%
      mutate(VISIT_DATE = as.Date(.data[[visit_date_col]])) %>%
      inner_join(ed_dates, by = "SUBJECT_ID") %>%
      filter(!is.na(VISIT_DATE), !is.na(ED_DATE))
    
    filtered <- df %>%
      filter(VISIT_DATE > ED_DATE) %>%
      {
        if ("VISITNUM" %in% names(.)) {
          filter(., !(VISITNUM %in% c("999", "997", "801", "802", "803")))
        } else .
      }
    
    if (nrow(filtered) == 0) {next}
    
    raw <- raw_process(filtered, "Visit occurred after ED date") %>% 
      select(-VISIT_DATE, -ED_DATE)
    
    if (!(ds_name %in% names(wb))) {
      addWorksheet(wb, ds_name)
      start_row <- 1
      write_header <- TRUE
    } else {
      existing_df <- tryCatch(readWorkbook(wb, sheet = ds_name), error = function(e) NULL)
      if (is.null(existing_df)) {
        start_row <- 1
        write_header <- TRUE
      } else {
        start_row <- nrow(existing_df) + 2
        write_header <- FALSE
      }
    }
    writeData(wb, sheet = ds_name, x = raw, startRow = start_row, colNames = write_header)
  }
  
  invisible(NULL)
}


##### 3.4 check overlapped visits #####
check_visit_overlap <- function(datasets_pool, wb,
                                dataset_no = NULL,
                                visit_info_df = NULL,
                                output_tab = NULL){
  
  datasets <- setdiff(names(datasets_pool), dataset_no)
  
  for (ds_name in datasets) {
    
    ds <- datasets_pool[[ds_name]]
    if (is.null(ds) || nrow(ds) == 0) {
      message("Dataset empty or NULL: ", ds_name)
      next
    }
    
    info_row <- visit_info_df %>% filter(dataset == ds_name)
    
    # Skip if no row found in visit_info_df
    if (nrow(info_row) == 0) {
      message("Skip: ", ds_name, " - not found in visit_info_df.")
      next
    }
    
    visit_date_col <- info_row$visit_date_col
    visit_label_col <- info_row$visit_label_col
    
    if (is.na(visit_date_col) | !(visit_date_col %in% names(ds))) {
      warning("Skip: ", ds_name, " - visit_date_col not found.")
      next
    }
    
    if ("VISITNUM" %in% names(ds)) {
      ds <- ds %>% 
        mutate(VISIT_NUM = suppressWarnings(as.numeric(VISITNUM))) %>%
        filter(!is.na(VISIT_NUM), !(VISIT_NUM %in% c(999, 997)))
    } else if (!is.na(visit_label_col) && visit_label_col %in% names(ds)) {
      ds <- ds %>% 
        filter(!is.na(.data[[visit_label_col]]),
               !grepl("unscheduled", .data[[visit_label_col]], ignore.case = TRUE)) %>%
        mutate(
          VISIT_NUM = suppressWarnings(
            as.numeric(stringr::str_extract(.data[[visit_label_col]], "\\d+"))
          )
        ) %>%
        filter(!is.na(VISIT_NUM))
    } else {
      warning("Skip: ", ds_name, " - cannot determine VISITNUM.")
      next
    }
    
    ## Create visit_date column
    ds <- ds %>%
      mutate(VISIT_DATE = as.Date(.data[[visit_date_col]])) %>%
      filter(!is.na(VISIT_DATE))
    
    ## Create overlap flag
    ds <- ds %>% 
      group_by(SITEID, SUBJECT_ID) %>%
      arrange(VISIT_NUM, .by_group = TRUE) %>%
      mutate(
        PREV_DATE = dplyr::lag(VISIT_DATE),
        OVERLAP_FL = VISIT_DATE < PREV_DATE
      ) %>%
      ungroup()
    
    ## get overlapped rows
    overlap_rows <- ds %>% filter(OVERLAP_FL)
    
    ## get the rows prior to overlapped rows
    prior_rows <- ds %>%
      inner_join(
        overlap_rows %>%
          select(SITEID, SUBJECT_ID, CUR_VISITNUM = VISIT_NUM, PREV_DATE),
        by = c("SITEID", "SUBJECT_ID", "VISIT_DATE" = "PREV_DATE")
      )
    
    ## bind them together
    raw0 <- bind_rows(overlap_rows, prior_rows) %>%
      arrange(SITEID, SUBJECT_ID, VISIT_NUM, VISIT_DATE) %>%
      select(-PREV_DATE, -OVERLAP_FL, -VISIT_DATE, -CUR_VISITNUM, -VISIT_NUM)
    
    raw <- raw_process(raw0, "Overlapped visits: later visit has earlier date")
    
    if (nrow(raw) == 0) next

    if (!(ds_name %in% names(wb))) {
      addWorksheet(wb, ds_name)
      start_row <- 1
      write_header <- TRUE
    } else {
      existing_df <- tryCatch(readWorkbook(wb, sheet = ds_name), error = function(e) NULL)
      if (is.null(existing_df)) {
        start_row <- 1
        write_header <- TRUE
      } else {
        start_row <- nrow(existing_df) + 2
        write_header <- FALSE
      }
    }
    writeData(wb, sheet = ds_name, x = raw, startRow = start_row, colNames = write_header)
  }
}
  
  


##### General Function #####
# run_common_checks <- function(dataset_names, wb, target_vars){
#   summary_list <- list()
#   raw_by_ds    <- list()
#   
#   for(ds_name in dataset_names){
#     ds <- get(ds_name, envir = .GlobalEnv)
#     
#     # 3.1 missing key vars
#     out1 <- check_missing_key_vars(ds_name, target_vars)
#     
#     # 3.2 duplicate visit date
#     label_col <- get_visit_label_col(ds)
#     date_col  <- get_visit_date_col(ds, ds_name)
# 
#     out2 <- if(!is.na(label_col) && !is.na(date_col)) check_duplicate_visit_date(ds, ds_name) else NULL
#     # 
#     # 
#     # # 3.3 visit after ED
#     # out3 <- if(!is.null(ds6001) && "VISIT_DATE" %in% names(ds)) check_visit_after_ED(ds, ed_ds = ds6001) else NULL
#     # 
#     # # # 3.4 overlap
#     # out4 <- if("VISITNUM"%in% names(ds) && !is.na(date_col)) check_overlap_visit(ds, date_col) else NULL
#     
#     outs <- list(out1, out2)
#     for(o in outs){
#       if(is.null(o)) next
#       summary_list[[length(summary_list)+1]] <- o$summary
#       raw_by_ds[[ds_name]] <- bind_rows(raw_by_ds[[ds_name]], o$raw)
#     }
#   }
#   
#   # Issue Log
#   if(length(summary_list)){
#     issue_log <- bind_rows(summary_list)
#     addWorksheet(wb, "Issue Log", tabColour="#92D050")
#     writeData(wb,"Issue Log",issue_log)
#   }
#   # sheets for specific dataset(s)
#   for(ds in names(raw_by_ds)){
#     addWorksheet(wb,ds)
#     writeData(wb,ds,raw_by_ds[[ds]])
#   }
#   invisible(NULL)
# }
# 





