
# scripts/04_check_sv_not_in_ds6001.R
# ----- Checkpoint: Regulatory visits (1-28, 801, 802) in sv1001 but not in ds6001

# 2.3 Visits in SV1001 but not in DS6001
## check visits (1-28, 801, 802)
check_sv_not_in_ds6001 <- function(datasets_pool, wb, other_datasets = NULL, 
                                   output_tab = NULL, visit_info_df = NULL,
                                   sv_visit = c(1:28,801,802)) {
  
  ## all datasets should exist
  req_ds   <- c("sv1001", "ds6001")
  missing_ds <- setdiff(req_ds, names(datasets_pool))
  if (length(missing_ds) > 0) {
    warning("Skip SV1001 vs DS6001 check: Missing dataset(s) ", paste(missing_ds, collapse = ", "))
    return(invisible(NULL))
  }
  
  if (is.null(visit_info_df) || !"visit_label_col" %in% names(visit_info_df) || !"dataset" %in% names(visit_info_df)) {
    warning("Skip check: visit_info_df is NULL or missing required columns: dataset, visit_label_col")
    return(invisible(NULL))
  }
  
  sv1001 <- datasets_pool[["sv1001"]]
  ds6001 <- datasets_pool[["ds6001"]]
  
  sv_visit_label_var <- visit_info_df %>%
    filter(dataset == "sv1001") %>%
    pull(visit_label_col) %>% 
    as.character()
  
  sv_visit_date_var <- visit_info_df %>%
    filter(dataset == "sv1001") %>%
    pull(visit_date_col) %>% 
    as.character()
  
  ds_visit_label_var <- visit_info_df %>%
    filter(dataset == "ds6001") %>%
    pull(visit_label_col) %>% 
    as.character()
  
  ## visit label column should exist
  if (length(sv_visit_label_var) == 0 || is.na(sv_visit_label_var) || !(sv_visit_label_var %in% colnames(sv1001))) {
    warning("Skip SV1001 vs DS6001 check: Missing visit label column in sv1001")
    return(invisible(NULL))
  }
  if (length(ds_visit_label_var) == 0 || is.na(ds_visit_label_var) || !(ds_visit_label_var %in% colnames(ds6001))) {
    warning("Skip SV1001 vs DS6001 check: Missing visit label column in ds6001")
    return(invisible(NULL))
  }
  
  ## check required variables
  check_required <- function(df, need, nm){
    miss <- setdiff(need, colnames(df))
    if (length(miss)) {
      warning(glue("Skip SV1001 vs DS6001 check: Missing column(s) in {nm}: {paste(miss, collapse=', ')}"))
      return(FALSE)  
    }
    return(TRUE)    
  }
  if (!check_required(sv1001, c("SUBJECT_ID", sv_visit_label_var, "VISITOCCUR"), "sv1001")) return(invisible(NULL))
  if (!check_required(ds6001, c("SUBJECT_ID", ds_visit_label_var, "FORMEID", "DSSTDAT"), "ds6001")) return(invisible(NULL))
  
  ##### patterns for the visits: varied by studies #####
  pattern_nums <- paste0(sv_visit, collapse = "|")
  pattern <- paste0("^evV(", pattern_nums, ")$")
  
  sv_filtered <- sv1001 %>%
    filter(
      grepl(pattern, .data[[sv_visit_label_var]], ignore.case = TRUE),
      VISITOCCUR == "Y"
    ) %>%
    select(SUBJECT_ID, SITEID, all_of(sv_visit_label_var)) %>%
    distinct()
  
  ds_filtered <- ds6001 %>%
    filter(
      grepl(pattern, .data[[ds_visit_label_var]], ignore.case = TRUE)
    ) %>%
    select(SUBJECT_ID, SITEID, all_of(ds_visit_label_var)) %>%
    distinct()
  
  sv_filtered <- sv_filtered %>% rename(EVENT = all_of(sv_visit_label_var))
  ds_filtered <- ds_filtered %>% rename(EVENT = all_of(ds_visit_label_var))
  
  missing_visits <- sv_filtered %>%
    anti_join(ds_filtered, by = c("SUBJECT_ID", "EVENT")) %>%
    arrange(SUBJECT_ID) %>%
    mutate(
      Issue_type                 = "Automatic",
      Issue_noted_by_Lilly_Stats = paste0("Patient had ", EVENT, " in SV1001, but not in DS6001"),
      PPD_Comment_or_resolution  = "",
      Status                     = "New"
    )
  
  # ## 06AUG2025 Added: if a patients' w52 visit date (Visit28) in sv1001 is later than ds6001_lv6 date, that is an issue
  # sv_w52 <- sv1001 %>%
  #   filter(.data[[sv_visit_label_var]] == "Visit 28", !is.na(.data[[sv_visit_date_var]])) %>%
  #   select(SUBJECT_ID, all_of(sv_visit_label_var), SV_EVENTDT = .data[[sv_visit_date_var]])
  # 
  # ds_lv6 <- ds6001 %>%
  #   filter(FORMEID == "DS6001_LV6", !is.na(DSSTDAT6)) %>%
  #   select(SUBJECT_ID, DS_EVENTDT = DSSTDAT6)
  # 
  # issue_w52_after_lv6 <- sv_w52 %>%
  #   inner_join(ds_lv6, by = "SUBJECT_ID") %>%
  #   mutate(SV_EVENTDT = as.Date(SV_EVENTDT),
  #          DS_EVENTDT = as.Date(DS_EVENTDT)) %>%
  #   filter(SV_EVENTDT > DS_EVENTDT)
  # 
  # library(glue)
  # 
  # issue_formatted <- issue_w52_after_lv6 %>%
  #   mutate(
  #     Issue_type = "Automatic",
  #     Issue_noted_by_Lilly_Stats = glue("Patient's w52(v28) date in sv1001: {SV_EVENTDT} is later than ds6001_lv6 date ({DS_EVENTDT})"),
  #     PPD_Comment_or_resolution = "",
  #     Status = "New"
  #   ) %>%
  #   select(SUBJECT_ID, EVENT, Issue_type, Issue_noted_by_Lilly_Stats, PPD_Comment_or_resolution, Status)
  # 
  # missing_visits <- bind_rows(missing_visits, issue_formatted)
  
  addWorksheet(wb, output_tab, tabColour = "#FFFF99")
  writeData(wb, sheet = output_tab, x = missing_visits)
  
  invisible(NULL)
}



