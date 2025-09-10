
# functions/201_check_visit_discrepancy.R

## Two sub-functions:
## 1) Join ex1001, sv1001, trtdspn (by VISID), find missing visits records
## 2) Join ex1001, sv1001, trtdspn (by VISID), find visit date mismatches

## Note: In KIAC, ex1001 didn't exist, using ec1001 instead

check_visit_discrepancy <- function(datasets_pool, wb,
                                    seq = NULL,
                                    output_tab = NULL, 
                                    visit_info_df = NULL,
                                    visit_excl = c(1,28,801,802,803)) {
  
  ds_ex  <- "ec1001"
  ds_sv  <- "sv1001"
  ds_trt <- "trtdspn"
  
  ec1001   <- datasets_pool[[ds_ex]]
  sv1001   <- datasets_pool[[ds_sv]]
  trtdspn  <- datasets_pool[[ds_trt]]
  ds6001 <- datasets_pool[["ds6001"]]
  
  ex_label_var  <- visit_info_df %>% filter(dataset == ds_ex) %>% pull(visit_label_col)
  ex_date_var   <- visit_info_df %>% filter(dataset == ds_ex) %>% pull(visit_date_col)
  
  sv_label_var  <- visit_info_df %>% filter(dataset == ds_sv) %>% pull(visit_label_col)
  sv_date_var   <- visit_info_df %>% filter(dataset == ds_sv) %>% pull(visit_date_col)
  
  if (is.na(ex_label_var) | !(ex_label_var %in% colnames(ec1001)) | 
      is.na(ex_date_var)  | !(ex_date_var %in% colnames(ec1001))) {
    warning("Skipped visit check: ec1001 missing required columns.")
    return(invisible(NULL))
  }
  if (is.na(sv_label_var) | !(sv_label_var %in% colnames(sv1001)) | 
      is.na(sv_date_var)  | !(sv_date_var %in% colnames(sv1001))) {
    warning("Skipped visit check: sv1001 missing required columns.")
    return(invisible(NULL))
  }
  
  ex_clean <- ec1001 %>%
    select(SUBJECT_ID, SITEID, all_of(ex_label_var), all_of(ex_date_var), ECOCCUR) %>%
    mutate(
      VISID = as.numeric(str_extract(.data[[ex_label_var]], "\\d+")),
      date = .data[[ex_date_var]],
      SUBJECT_ID = as.character(SUBJECT_ID)
    ) %>%
    rename(event_ex = all_of(ex_label_var)) %>%
    select(SUBJECT_ID, VISID, date, event_ex, ECOCCUR)
  
  sv_clean <- sv1001 %>%
    select(SUBJECT_ID, all_of(sv_label_var), all_of(sv_date_var)) %>%
    mutate(
      VISID = as.numeric(str_extract(.data[[sv_label_var]], "\\d+")),
      date = .data[[sv_date_var]],
      SUBJECT_ID = as.character(SUBJECT_ID)
    ) %>%
    rename(event_sv = all_of(sv_label_var)) %>%
    select(SUBJECT_ID, VISID, date, event_sv)
  
  trt_clean <- trtdspn %>%
    select(SUBJECT_ID, PKGDTTM, VISID) %>%
    mutate(
      date = as.Date(PKGDTTM),
      SUBJECT_ID = as.character(SUBJECT_ID)
    ) %>%
    distinct(SUBJECT_ID, VISID, date, .keep_all = TRUE) 
  
  merged <- full_join(ex_clean, sv_clean, by = c("SUBJECT_ID", "VISID", "date")) %>%
    full_join(trt_clean %>% select(-date), by = c("SUBJECT_ID", "VISID"))
  
  ## Exclusion list from ds6001: subjects not randomized
  non_randomized_subjects <- ds6001 %>%
    filter(DSCONT_RANDTRT == "N") %>%
    pull(SUBJECT_ID) %>%
    as.character()
  
  visit_issues1 <- merged %>%
    filter(!VISID %in% visit_excl) %>%
    filter(is.na(ECOCCUR) | ECOCCUR == "Y") %>%
    filter(is.na(event_ex) | is.na(event_sv) | is.na(VISID) | is.na(PKGDTTM)) %>%
    filter(!SUBJECT_ID %in% non_randomized_subjects) %>%
    rowwise() %>%
    mutate(Issue_type = "Automatic",
           Issue_noted_by_Lilly_Stats = {
             missing_fields <- c()
             if (is.na(event_ex)) missing_fields <- c(missing_fields, "EC1001")
             if (is.na(event_sv)) missing_fields <- c(missing_fields, "SV1001")
             if (is.na(PKGDTTM))  missing_fields <- c(missing_fields, "TRTDSPN")
             if (is.na(VISID))    missing_fields <- c(missing_fields, "VISID")
             paste0("Missing visit(s) in ", paste(missing_fields, collapse = ", "))
           },
           PPD_Comment_or_resolution = "",
           Status = "New") %>%
    ungroup() %>%
    arrange(SUBJECT_ID, VISID, date)
  
  visit_issues1 <- convert_dates_to_char(visit_issues1)
  
  ## Visit discrepancy
  visit_issues2 <- merged %>%
    filter(!VISID %in% visit_excl) %>%
    filter(!ECOCCUR %in% c("", "N")) %>%
    filter(event_sv != "Unscheduled visit") %>%
    filter(!SUBJECT_ID %in% non_randomized_subjects) %>%
    rowwise() %>%
    filter(
      is.na(event_ex) | is.na(event_sv) | is.na(VISID) | is.na(PKGDTTM) |
        (!is.na(date) & !is.na(PKGDTTM) & date != as.Date(PKGDTTM))
    ) %>%
    # filter(as.Date(PKGDTTM) > date) %>%  #JB 26JUN2025: exclude records having PKGDTTM < EX/SV visit date
    mutate(
      Issue_type = "Automatic",
      Issue_noted_by_Lilly_Stats = "PKGDTTM (in TRTDSPN) later than EX/SV visit date",
      PPD_Comment_or_resolution = "",
      Status = "New"
    ) %>%
    ungroup() %>%
    arrange(SUBJECT_ID, VISID, date)
  
  if(seq == 1){
    addWorksheet(wb, output_tab, tabColour = "#FFFF99")
    writeData(wb, sheet = output_tab, x = visit_issues1)
  }
  
  if(seq == 2){
    addWorksheet(wb, output_tab, tabColour = "#FFFF99")
    writeData(wb, sheet = output_tab, x = visit_issues2)
  }

  invisible(NULL)
}


