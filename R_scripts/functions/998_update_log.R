
update_issue_log <- function(wb, master_path) {
  ## sheet_keys: self-defined
  sheet_keys <- list(
    # "ex1001" = c("SUBJID", "COUNTRY", "SITENUM", "EGROUP", "EVENT", "EVENTDT", "EXOCCUR", "EXFLRTU_EX1001_F8"),
    # "cm1001" = c("SUBJID", "COUNTRY", "SITENUM", "EVENT", "CMTRT", "CMSTDAT", "PREFLABEL"),
    # "cm2001" = c("SUBJID", "COUNTRY", "SITENUM", "EVENT", "CMTRT", "CMSTDAT", "PREFLABEL"),
    # "ds6001" = c("SUBJID", "COUNTRY", "SITENUM", "EVENT")
  )
  
  if (file.exists(master_path)) {
    # master_log <- read_excel(master_path, 
    #                          sheet = "Issue Log",
    #                          col_types = "text")
    
    ## Other sheets
    sheets <- names(wb)
    sheets <- sheets[!sheets %in% c("READ_ME")]
    for (sheet in sheets){
      new <- readWorkbook(wb, sheet = sheet, detectDates = FALSE) %>% 
        select(-PPD_Comment_or_resolution)
      new[] <- lapply(new, as.character)
      
      new <- new %>% 
        mutate(across(everything(), ~ na_if(.x, ""))) # turn empty string into NA
      
      if (!(sheet %in% excel_sheets(master_path))) {next} ## sheet exists in new but not in old
      
      ## fill down NAs because of cells merge
      fill_cols <- c("Issue_noted_by_Lilly_Stats", "Status", "Issue_type", "PPD_Comment_or_resolution")
      old <- read_excel(master_path, sheet = sheet, col_types = "text") %>% 
        mutate(row_num = row_number()) %>% 
        fill(all_of(fill_cols), .direction = "down")
      
      
      required_cols <- c("SUBJECT_ID", "Issue_noted_by_Lilly_Stats", "Issue_type", "Status")
      if (!all(required_cols %in% colnames(new))) next
      
      # old: Manual, Automatic
      old_manual <- old %>% filter(Issue_type == "Manual")
      old_auto   <- old %>% filter(Issue_type == "Automatic" | is.na(Issue_type))
      
      # new: automatic
      new_auto <- new
      
      
      if (!is.null(sheet_keys[[sheet]])){
        candidate_keys <- sheet_keys[[sheet]]
        common_keys <- intersect(candidate_keys, intersect(names(old_auto), names(new_auto)))
        
        if (length(common_keys) == 0) {
          warning(paste0("No valid join keys found for sheet: ", sheet))
          next
        }
        
        by_cols <- common_keys
      } else{
        common_cols <- intersect(names(old_auto), names(new_auto))
        by_cols <- setdiff(common_cols, c("Status", "Issue_noted_by_Lilly_Stats", "Issue_type"))
      }
      
      merged_auto <- full_join(old_auto, new_auto, by = by_cols, suffix = c("_old", "_new"))
      
      today_tag <- format(Sys.Date(), "%d%b%Y") %>% toupper()
      
      updated_auto <- merged_auto %>%
        mutate(
          Status = case_when(
            str_detect(tolower(Status_old), "permanent") ~ Status_old,
            is.na(Status_old) & is.na(Status_new) ~ "Closed",
            is.na(Status_old) & !is.na(Status_new) ~ "New",
            !is.na(Status_old) & !is.na(Status_new) & Status_old == "Closed" ~ "Recurred",
            !is.na(Status_old) & !is.na(Status_new) ~ "Open",
            !is.na(Status_old) & is.na(Status_new) ~ "Closed",
            TRUE ~ Status_old 
          ),
          Issue_type = case_when(
            !is.na(Issue_type_new) ~ Issue_type_new,
            is.na(Issue_type_new) & !is.na(Issue_type_old) ~ Issue_type_old,
            TRUE ~ NA_character_
          ),
          Issue_noted_by_Lilly_Stats = case_when(
            is.na(Status_new) | str_detect(tolower(Status_old), "permanent") ~ Issue_noted_by_Lilly_Stats_old,
            is.na(Issue_noted_by_Lilly_Stats_new) ~ Issue_noted_by_Lilly_Stats_old,
            is.na(Issue_noted_by_Lilly_Stats_old) ~ paste0(today_tag, ": ", Issue_noted_by_Lilly_Stats_new),
            TRUE ~ paste(
              Issue_noted_by_Lilly_Stats_old,
              paste0(today_tag, ": ", Issue_noted_by_Lilly_Stats_new),
              sep = "\n"
            )
          )) %>% 
        select(-Status_new, -Status_old, -Issue_noted_by_Lilly_Stats_old, -Issue_noted_by_Lilly_Stats_new,
               -Issue_type_new, -Issue_type_old)
      
      trailing_cols <- c("Issue_type", "Issue_noted_by_Lilly_Stats", "PPD_Comment_or_resolution", "Status")
      other_cols <- setdiff(names(updated_auto), trailing_cols)
      
      updated_auto <- updated_auto %>% 
        select(all_of(c(other_cols, trailing_cols))) %>% 
        select(-matches("_old$"))
      names(updated_auto) <- gsub("_new$", "", names(updated_auto))
      
      
      updated_sheet <- bind_rows(updated_auto, old_manual) %>% 
        arrange(row_num) %>% 
        select(-row_num)
      
      writeData(wb, sheet, updated_sheet)
      
    }
    
    ## Last section: Keep all the old sheets
    old_sheets <- excel_sheets(master_path)
    old_sheets <- old_sheets[!old_sheets %in% c("READ_ME")]
    
    missing_sheets <- setdiff(old_sheets, names(wb))
    
    for (sheet in missing_sheets) {
      old_data <- read_excel(master_path, sheet = sheet, col_types = "text")
      addWorksheet(wb, sheet)
      writeData(wb, sheet = sheet, old_data)
    }
    
  } else {
    master_log <- data.frame(
      dataset_name = character(),
      SITEID = character(),
      SUBJECT_ID = character(),
      issue = character(),
      first_detected_date = character(),
      last_checked_date = character(),
      status = character(),
      manual_comment = character(),
      stringsAsFactors = FALSE
    )
  }
  
  # merge and update status
  # combined_issues <- combined_issues %>%
  #   dplyr::mutate(dplyr::across(c(FIRST_DETECTED_DATE, LAST_CHECKED_DATE), as.character))
  # 
  # merged <- dplyr::full_join(master_log, combined_issues,
  #                            by = c("DATASET", "SITEID", "SUBJECT_ID", "ISSUE"),
  #                            suffix = c("_OLD", "_NEW"))
  # 
  # updated_log <- merged %>%
  #   dplyr::mutate(
  #     STATUS = dplyr::case_when(
  #       is.na(STATUS_OLD) & !is.na(STATUS_NEW) ~ "New",
  #       !is.na(STATUS_OLD) & !is.na(STATUS_NEW) & STATUS_OLD == "Closed" ~ "Recurred",
  #       !is.na(STATUS_OLD) & !is.na(STATUS_NEW) ~ "Open",
  #       !is.na(STATUS_OLD) & is.na(STATUS_NEW) ~ "Closed",
  #       TRUE ~ STATUS_OLD
  #     ),
  #     FIRST_DETECTED_DATE = coalesce(FIRST_DETECTED_DATE_OLD, FIRST_DETECTED_DATE_NEW),
  #     LAST_CHECKED_DATE   = as.character(Sys.Date())
  #   ) %>%
  #   dplyr::select(DATASET, SITEID, SUBJECT_ID, ISSUE,
  #                 FIRST_DETECTED_DATE, LAST_CHECKED_DATE, STATUS)
  # 
  # openxlsx::writeData(wb, "Issue Log", updated_log)
  
  return(wb)
}
