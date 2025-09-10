## duplicated visits within the same date - customized code list

custom_code_list = list(
  # 
  lab = function(df, visit_date_col) {
    df[[visit_date_col]] <- as.Date(substr(df[[visit_date_col]], 1, 10))

    df %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col), LBSPEC, PRTEST, PRTESTCD) %>%
      mutate(has_retest = any(VISIT == "Retest")) %>%
      filter((n_distinct(VISIT) > 1 | n() > 1) & !(VISIT == "Retest" & has_retest)) %>%
      select(-has_retest) %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col)) %>%
      filter(n() > 1) %>%
      ungroup()

  },
  
  sys_form =  function(df, visit_date_col) {
      df %>%
          group_by(SUBJECT_ID, !!sym(visit_date_col), FORMEID) %>%
          filter(n() > 1) %>%
          ungroup()
  },
  
  yprime_gadvas2001 =  function(df, visit_date_col) {
    df %>%
      group_by(SUBJECT_ID, !!sym(visit_date_col), GADVASEVAL) %>%
      filter(n() > 1) %>%
      ungroup()
  }
  
  # 
  # i7pmcdsaf_lesion = function(df, visit_date_col) {
  #   df %>%
  #     group_by(SUBJECT_ID, !!sym(visit_date_col), DLALOC, DLALAT, DLADIR, DLARES1) %>%
  #     filter(n() > 1) %>%
  #     ungroup()
  # },
  # 
  # ds2001 = function(df, visit_date_col) {
  #   df %>%
  #     group_by(SUBJECT_ID, !!sym(visit_date_col), FORMEID) %>%
  #     filter(n() > 1) %>%
  #     ungroup()
  # },
  # 
  # sys_form = function(df, visit_date_col) {
  #   df %>%
  #     group_by(SUBJECT_ID, !!sym(visit_date_col), FORMEID) %>%
  #     filter(n() > 1) %>%
  #     ungroup()
  # },
  # 
  # ds6001 = function(df, visit_date_col) {
  #   df %>%
  #     group_by(SUBJECT_ID, !!sym(visit_date_col), FORMEID) %>%
  #     filter(n() > 1) %>%
  #     ungroup()
  # },
  # 
  # cm2001 = function(df, visit_date_col) {
  #   df %>%
  #     group_by(SUBJECT_ID, !!sym(visit_date_col), FORMEID, CMYN, CMTRT, CMSTDAT, IGSEQ) %>%
  #     filter(n() > 1) %>%
  #     ungroup()
  # },
  # 
  # i7pmcdsaf_itchnrs = function(df, visit_date_col) {
  #   df %>%
  #     group_by(SUBJECT_ID, !!sym(visit_date_col), INRSASMTIM) %>%
  #     filter(n() > 1 & VISIT == "DAILY DIARY") %>%
  #     ungroup()
  # },
  # 
  # mhpresp1001 = function(df, visit_date_col) {
  #   df %>%
  #     group_by(SUBJECT_ID, !!sym(visit_date_col), FORMEID, HLT, PT, LLT) %>%
  #     filter(n() > 1) %>%
  #     ungroup()
  # },
  # 
  # pr1001 = function(df, visit_date_col) {
  #   df %>%
  #     group_by(SUBJECT_ID, !!sym(visit_date_col), FORMEID, PRLOC_PR1001_F1_D, LOCATION_SIDE_D, PRDIR_D) %>%
  #     filter(n() > 1) %>%
  #     mutate(
  #       f1_all_not_na = all(!is.na(PRSTDAT_PR1001_F1)),
  #       f1_has_duplicates = length(unique(PRSTDAT_PR1001_F1)) < n(),
  #       f2_all_not_na = all(!is.na(PRSTDAT_PR1001_F2)),
  #       f2_has_duplicates = length(unique(PRSTDAT_PR1001_F2)) < n()
  #     ) %>%
  #     filter(
  #       case_when(
  #         f1_all_not_na ~ f1_has_duplicates,
  #         !f1_all_not_na & f2_all_not_na ~ f2_has_duplicates,
  #         TRUE ~ TRUE
  #       )
  #     ) %>%
  #     select(-f1_all_not_na, -f1_has_duplicates, -f2_all_not_na, -f2_has_duplicates) %>%
  #     ungroup()
  # },
  # 
  # sv1001 = function(df, visit_date_col) {
  #   df %>%
  #     group_by(SUBJECT_ID, !!sym(visit_date_col)) %>%
  #     filter(!(VISITOCCUR == "N")) %>%
  #     filter(n() > 1) %>%
  #     ungroup()
  # },
  # 
  # vs1001 = function(df, visit_date_col) {
  #   df %>%
  #     group_by(SUBJECT_ID, !!sym(visit_date_col)) %>%
  #     filter(FORMILB == "N" & VSPERF == "Y") %>%
  #     filter(n() > 1) %>%
  #     ungroup()
  # },
  # 
  # i7pmcdsaf_hsnrs = function(df, visit_date_col) {
  #   df %>%
  #     group_by(SUBJECT_ID, !!sym(visit_date_col)) %>%
  #     filter(
  #       n() > 1 &
  #         (any(VISIT != "DAILY DIARY", na.rm = TRUE) |
  #            all(VISIT != "DAILY DIARY", na.rm = TRUE))
  #     ) %>%
  #     ungroup()
  # },
  # 
  # lb9001 = function(df, visit_date_col) {
  #   df %>%
  #     group_by(SUBJECT_ID, !!sym(visit_date_col)) %>%
  #     filter(!(EVENT == "Hepatic Event 1" & n() > 1)) %>%
  #     ungroup()
  # },
  # 
  # apmh1001 = function(df, visit_date_col) {
  #   df %>%
  #     group_by(SUBJECT_ID, !!sym(visit_date_col)) %>%
  #     filter(!(EVENT == "Hepatic Event 1" & n() > 1)) %>%
  #     ungroup()
  # },
  # 
  # hmpr1001 = function(df, visit_date_col) {
  #   df %>%
  #     group_by(SUBJECT_ID, !!sym(visit_date_col)) %>%
  #     filter(!(EVENT == "Hepatic Event 1" & n() > 1)) %>%
  #     ungroup()
  # }
)
