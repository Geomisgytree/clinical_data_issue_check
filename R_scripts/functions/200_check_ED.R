# scripts/02_check_ED.R

## Description:
# Patients having ED visit in DS6001, but no ED visits in lab or eCOA datasets 

## Note:
# Need to make sure that at least the two datasets (ds6001, lab) exist.
# Find all the variables needed in the two datasets 



check_ED <- function(datasets_pool, 
                     wb, 
                     other_datasets = NULL, 
                     output_tab = NULL, 
                     visit_info_df = NULL) {
  
  ds6001 <- datasets_pool[["ds6001"]]
  sv1001 <- datasets_pool[["sv1001"]]
  
  if (is.null(ds6001) | is.null(sv1001)) {
    warning("ds6001 or sv1001 missing. Skipped.")
    return(invisible(NULL))
  }
  
  # ds6001 must include variable DSSTDAT6, SUBJECT_ID
  req_vs <- c("DSSTDAT", "SUBJECT_ID")
  miss_vs <- setdiff(req_vs, names(ds6001))
  
  if (length(miss_vs) > 0) {
    warning("ED check skipped: ds6001 lacks variable(s) ", paste(miss_vs, collapse = ", "))
    return(invisible(NULL))
  }
  
  ## Find ED date in ds6001
  ED_DS <- ds6001 %>%
    filter(FORMEID %in% c("DS6001_LV6", "DS6001_LV5") & DSSTDAT != "") %>%
    mutate(DS_ED = as.Date(DSSTDAT)) %>% 
    transmute(SUBJECT_ID, SITEID, DS_ED = as.character(DS_ED))
  
  available_others <- intersect(other_datasets, names(datasets_pool))
  if (length(available_others) == 0) {
    warning("No valid other datasets found in other_datasets. Skipped.")
    return(invisible(NULL))
  }
  
  ed_data_list <- list()
  
  ## Find ED date in other datasets
  for (ds_name in available_others){
    df <- datasets_pool[[ds_name]]
    info_row <- visit_info_df %>% filter(dataset == ds_name)
    if (nrow(info_row) == 0) {
      warning("Dataset ", ds_name, " not found in visit_info_df. Skipped.")
      next
    }
    visit_col <- info_row$visit_label_col 
    date_col  <- info_row$visit_date_col
    
    if (is.na(visit_col) || is.na(date_col)) {
      warning("Dataset ", ds_name, " missing visit/date column info. Skipped.")
      next
    }
    
    # Check if both visit_col and date_col exist in dataset
    if (!visit_col %in% names(df)) {
      message("Skipped ", ds_name, ": visit_col '", visit_col, "' not found in dataset.")
      next
    }
    if (!date_col %in% names(df)) {
      message("Skipped ", ds_name, ": date_col '", date_col, "' not found in dataset.")
      next
    }
    
    # Filter for ED visits: ED/EARLY DISCONTINUATION/"999"
    ed_sub <- df %>%
      filter(str_detect(.data[[visit_col]], "(?i)\\bED\\b|EARLY DISCONTINUATION|\\b999\\b")) 
    
    if (nrow(ed_sub) == 0) {
      message("Skipped ", ds_name, ": no rows with ED-related VISIT values.")
      next
    }
    
    # Try selecting and renaming
    ed_sub <- ed_sub %>%
      select(SUBJECT_ID, all_of(date_col)) %>%
      mutate(!!date_col := as.Date(.data[[date_col]])) %>%
      distinct()
    
    new_col_name <- paste0(ds_name, "_ED")
    names(ed_sub)[names(ed_sub) == date_col] <- new_col_name
    ed_sub <- ed_sub %>% mutate(across(all_of(new_col_name), as.character))
    
    # Save into ed_data_list
    ed_data_list[[ds_name]] <- ed_sub
    message("Included ", ds_name, ": ", nrow(ed_sub), " ED record(s) as ", new_col_name, ".")
    
  }
  
  ## Combine all the ED info
  if (length(ed_data_list) > 0) {
    ED_OTHERS <- reduce(ed_data_list, full_join, by = "SUBJECT_ID")
  } else {
    ED_OTHERS <- tibble(SUBJECT_ID = ED_DS$SUBJECT_ID)
  }

  
  ED_full <- ED_DS %>%
    full_join(ED_OTHERS, by = "SUBJECT_ID") %>% 
    distinct(SUBJECT_ID, .keep_all = TRUE)
  
  ed_cols <- setdiff(names(ED_full), c("SUBJECT_ID", "SITEID"))
    
  ED_cleaned <- ED_full %>%
    mutate(across(all_of(ed_cols), ~ as.Date(.))) %>% 
    filter(!(if_all(all_of(ed_cols), ~ !is.na(.)) &
               apply(select(., all_of(ed_cols)), 1, function(x) length(unique(x)) == 1))) %>%
    mutate(Issue_type = "Automatic",
           PPD_Comment_or_resolution = "",
           Status = "New") %>%
    mutate(row_id = row_number()) %>%
    pivot_longer(cols = all_of(ed_cols), names_to = "colname", values_to = "date_val") %>%
    mutate(dataset = str_remove(colname, "_ED$")) %>%
    group_by(row_id) %>%
    summarise(
      SUBJECT_ID = first(SUBJECT_ID),
      ed_info = {
        row_vals <- cur_data()
        non_na_vals <- row_vals$date_val[!is.na(row_vals$date_val)]
        
        # see if there's discrepancy of ED dates
        has_date_diff <- length(unique(non_na_vals)) > 1
        
        missing_datasets <- row_vals$dataset[is.na(row_vals$date_val)]
        has_missing <- length(missing_datasets) > 0
        
        # combine two situations
        if (has_date_diff & has_missing) {
          paste0(
            "ED date difference across ds6001 and other datasets; ",
            "Missing ED in ", paste(missing_datasets, collapse = ", "), " dataset(s)"
          )
        } else if (has_date_diff) {
          "ED date difference across ds6001 and other datasets"
        } else if (has_missing) {
          paste0("Missing ED in ", paste(missing_datasets, collapse = ", "), " dataset(s)")
        } else {
          NA_character_
        }
      }
    ) %>%
    rename(Issue_noted_by_Lilly_Stats = ed_info) %>%
    right_join(ED_full %>%
                 filter(!(if_all(all_of(ed_cols), ~ !is.na(.)) &
                            apply(select(., all_of(ed_cols)), 1, function(x) length(unique(x)) == 1))) %>%
                 mutate(Issue_type = "Automatic",
                        PPD_Comment_or_resolution = "",
                        Status = "New") %>%
                 mutate(row_id = row_number()),
               by = c("row_id", "SUBJECT_ID")) %>%
    select(-row_id) %>% 
    select(SUBJECT_ID, SITEID, Issue_noted_by_Lilly_Stats, all_of(ed_cols), Issue_type, PPD_Comment_or_resolution, Status)
  
  ED_cleaned <- convert_dates_to_char(ED_cleaned)
  
  SITEID_lookup <- ds6001 %>% select(SUBJECT_ID, SITEID) %>% distinct()
  
  ED_cleaned <- ED_cleaned %>%
    select(-SITEID) %>%  
    left_join(SITEID_lookup, by = "SUBJECT_ID") %>%
    relocate(SITEID, .after = SUBJECT_ID) 
  # 
  # ## If a patient completed the study period, had visit 28, then he/she doesn't need to have ED records 
  # ## in other datasets (06AUG2025)
  # no_other_ed_needed_ids <- ds6001 %>%
  #   filter(FORMDEF == "DS6001_LV11", DSCONT_COMFNLTMT == 'Y') %>%
  #   pull(SUBJID)
  # 
  # ## If ED date earlier than V10_DATE, then no i7pmcdsaf_hsnrs_ED and i7pmcdsaf_itchnrs_ED are expected
  # ED_cleaned <- ED_cleaned %>%
  #   left_join(V10_date, by = "SUBJECT_ID") %>% 
  #   mutate(is_date_difference = if_else(
  #     str_detect(Issue_noted_by_Lilly_Stats, "ED date difference"),
  #     "Y", "N"
  #   )) %>% 
  #   filter(
  #     !(
  #       as.Date(DS_ED) < as.Date(V10_DATE) &
  #         is.na(i7pmcdsaf_hsnrs_ED) &
  #         is.na(i7pmcdsaf_itchnrs_ED) &
  #         if_all(
  #           -c(SUBJECT_ID, SITEID, Issue_noted_by_Lilly_Stats, DS_ED, lab_ED,
  #              i7pmcdsaf_hsnrs_ED, i7pmcdsaf_itchnrs_ED, Issue_type,
  #              PPD_Comment_or_resolution, Status, V10_DATE),
  #           ~ !is.na(.)
  #         ) 
  #     )
  #   ) %>%
  #   filter(
  #     !(SUBJECT_ID %in% no_other_ed_needed_ids) | ## New condition 06AUG2025: filter out unnecessary subjects
  #       is_date_difference == "Y" ## keep date difference rows
  #   ) %>%
  #   select(-V10_DATE)
  
  addWorksheet(wb, output_tab, tabColour = "#FFFF99")
  writeData(wb, sheet = output_tab, x = ED_cleaned)
  
  invisible(NULL)
}



