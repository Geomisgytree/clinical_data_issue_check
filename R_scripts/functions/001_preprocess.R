
preprocess_data <- function(dataset_names, preprocess_spec) {
  visit_info_list <- list() # Used 
  for (nm in dataset_names) {
    df <- get(nm, envir = .GlobalEnv)
    
    ## Read in spec
    spec_row <- preprocess_spec %>% filter(DATASET == nm & (is.na(REMOVE) | REMOVE != "Y")) 
    
    # ====================
    # 1) SUBJECT_ID
    # ====================
    if ("SUBJECT" %in% names(df)) {
      df <- df %>% mutate(SUBJECT_ID = sub(".*-", "", SUBJECT))
      subject_var <- "SUBJECT"
    } else if ("SUBJID" %in% names(df)) {
      df <- df %>% mutate(SUBJECT_ID = sub(".*-", "", SUBJID))
      subject_var <- "SUBJID"
    } else if ("USUBJID" %in% names(df)) {
      df <- df %>% mutate(SUBJECT_ID = sub(".*-", "", USUBJID))
      subject_var <- "USUBJID"
    } else if ("SUBJECTNAME" %in% names(df)) {
      df <- df %>% mutate(SUBJECT_ID = sub(".*-", "", SUBJECTNAME))
      subject_var <- "SUBJECTNAME"
    } else {
      df <- df %>% mutate(SUBJECT_ID = NA_character_)
      subject_var <- ""
    }
    
    # ====================
    # 2) SITEID
    # ====================
    if ("SITEID" %in% names(df)) {
      df <- df %>% mutate(SITEID = as.character(SITEID))
      site_var <- "SITEID"
    } else if ("SITENUM" %in% names(df)) {
      df <- df %>% mutate(SITEID = as.character(SITENUM))
      site_var <- "SITENUM"
    } else if ("USUBJID" %in% names(df)) {
      df <- df %>% mutate(SITEID = substr(USUBJID, 1, 5))
      site_var <- "USUBJID"
    } else if ("SITE" %in% names(df)){
      df <- df %>% mutate(SITEID = as.character(SITE))
      site_var <- "SITE"
    } else if ("SITENUMBER" %in% names(df)){
      df <- df %>% mutate(SITEID = as.character(SITENUMBER))
      site_var <- "SITENUMBER"
    } else if ("INVID" %in% names(df)){ ## TRTASPN/TRTDSPN: INVID for SITE ID
      df <- df %>% mutate(SITEID = as.character(INVID))
      site_var <- "INVID"
    } else {
      df <- df %>% mutate(SITEID = NA_character_)
      site_var <- ""
    }
    
    # ====================
    # 3) Customized row filter from EXCLUDE_ROWS
    # ====================
    if (nrow(spec_row) > 0 && !is.na(spec_row$EXCLUDE_ROWS)) {
      custom_filter <- spec_row$EXCLUDE_ROWS
      # Parse and evaluate as a dplyr filter condition
      df <- df %>% filter(eval(parse(text = custom_filter)))
    }
    
    # ====================
    # 4) Customized columns filter from EXCLUDE_COLS
    # ====================
    if (nrow(spec_row) > 0 && !is.na(spec_row$EXCLUDE_COLS)) {
      selected_cols <- strsplit(spec_row$EXCLUDE_COLS, ",")[[1]] %>% trimws()
      # Check columns exist, otherwise warn
      existing_cols <- selected_cols[selected_cols %in% names(df)]
      df <- df %>% select(all_of(existing_cols))
    }
    
    # ====================
    # 5) Visit date column: Define visit_date_col
    # ====================
    visit_date_col <- NULL
    if ("EVENTDT" %in% names(df)) { ## NO.1 choice: EVENTDT
      visit_date_col <- "EVENTDT"
    } 
    
    else if (nm == "lab" && "LBDTM" %in% names(df)) {
      visit_date_col <- "LBDTM" 
    } 
    else if (nm == "sv1001"){
      visit_date_col <- "VISDAT"
    }
    else if (nm == "vs1001"){
      visit_date_col <- "VSDAT"
    }
    else if ("ECOAASMDT" %in% names(df)){
      df <- df %>% mutate(ECOAASMDT = as.Date(ECOAASMDT))
      visit_date_col <- "ECOAASMDT"
    }
    else if (nm == "ec1001"){
      visit_date_col <- "ECSTDAT"
    }
    
    # ====================
    # 6) Visit label column: Define visit_label_col
    # ====================
    visit_label_col <- if ("VISIT" %in% names(df)) {
      "VISIT"
    } else if ("EVENT" %in% names(df)) {
      "EVENT"
    } else {
      NULL
    }

    visit_info_list[[nm]] <- data.frame(
      dataset = nm,
      subject_var = subject_var,
      site_var = site_var,
      visit_date_col = ifelse(is.null(visit_date_col), NA, visit_date_col),
      visit_label_col = ifelse(is.null(visit_label_col), NA, visit_label_col)
    )
    
    assign(nm, df, envir = .GlobalEnv)
  }

  visit_info_df <- do.call(rbind, visit_info_list)
  rownames(visit_info_df) <- NULL
  
  result <- list(
    visit_info_df = visit_info_df,
    visit_info_list = visit_info_list
  )
  return(result) ## data_info
  
}


## change variable formats to date
# lili7pmcdsaf_pkhead_p$LBDTM <- as.POSIXct(lili7pmcdsaf_pkhead_p$LBDTM, format = "%d%b%YT%H:%M:%S", tz = "UTC")
# lili7pmcdsaf_pkhead_p$LBDTM <- as.Date(lili7pmcdsaf_pkhead_p$LBDTM)