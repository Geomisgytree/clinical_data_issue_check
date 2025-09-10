# scripts/06_check_visit_gap.R

library(dplyr)
library(stringr)
library(openxlsx)
library(glue)

# 2.5 Consecutive visits should be >1 wk and <3 wks
check_visit_gap <- function(datasets_pool, wb,
                            visit_info_df = NULL, 
                            output_tab = NULL,
                            day_lob = 7,
                            day_hib = 21) {

  available_ds <- names(datasets_pool)
  
  visit_gap_issues <- list()
  
  for (ds_name in available_ds) {
    
    df <- datasets_pool[[ds_name]]
    info_row <- visit_info_df %>% filter(dataset == ds_name)
    
    if (nrow(info_row) == 0) {
      warning("Dataset ", ds_name, " not found in visit_info_df. Skipped.")
      next
    }
    
    # subject_col <- info_row$subject_var
    visit_col   <- info_row$visit_label_col
    date_col    <- info_row$visit_date_col
    
    ## skip the dataset if there's no visit or date column
    if (is.na(visit_col) || is.na(date_col)) {
      warning("Dataset ", ds_name, " missing visit/date column info. Skipped.")
      next
    }

    
    # Filter out effective visits
    tmp_df <- df %>%
      filter(!is.na(.data[[visit_col]])) %>%
      filter(
        grepl("(?i)^(evV)?\\d+$", .data[[visit_col]]) & ## match evVx, e.g., evV1, evV2, evV801, etc. / only numbers (1, 2, 801)
          !grepl("(?i)unscheduled", .data[[visit_col]]) 
      ) %>%
      transmute(
        SUBJECT_ID,
        VISIT      = .data[[visit_col]],
        VISIT_DATE = as.Date(.data[[date_col]])
      ) %>%
      arrange(SUBJECT_ID, VISIT_DATE)
    
    # Calculate consecutive visits
    tmp_df <- tmp_df %>%
      mutate(
        VISIT_NUM = as.numeric(str_extract(VISIT, "\\d+"))
      ) %>%
      filter(!is.na(VISIT_NUM)) %>%
      arrange(SUBJECT_ID, VISIT_NUM, VISIT_DATE) %>%
      group_by(SUBJECT_ID) %>%
      mutate(
        NEXT_VISIT_NUM  = lead(VISIT_NUM),
        NEXT_VISIT_DATE = lead(VISIT_DATE),
        DAYS_DIFF       = as.numeric(difftime(NEXT_VISIT_DATE, VISIT_DATE, units = "days")),
        WEEKS_DIFF      = round(DAYS_DIFF / 7, 1)
      ) %>%
      ungroup() %>%
      filter(!is.na(NEXT_VISIT_NUM),
             abs(NEXT_VISIT_NUM - VISIT_NUM) == 1,
             !VISIT_NUM %in% c(1, 801, 802, 803),  ## exclude special visits
             DAYS_DIFF <= day_lob | DAYS_DIFF >= day_hib) %>% ##### ENTER DAY GAP HERE
      mutate(
        issue = paste0("Visit ", VISIT_NUM, " and Visit ", NEXT_VISIT_NUM,
                       " have a ", WEEKS_DIFF, " wks (", DAYS_DIFF, " days) gap.")
      )
    
    # Collect problematic rows
    if (nrow(tmp_df) > 0) {
      visit_gap_issues[[ds_name]] <- tmp_df %>%
        select(SUBJECT_ID, VISIT, VISIT_DATE,
               NEXT_VISIT_NUM, NEXT_VISIT_DATE,
               DAYS_DIFF, WEEKS_DIFF, issue)
    }
  }
  
  if (length(visit_gap_issues) > 0) {
    final_issues <- bind_rows(visit_gap_issues, .id = "dataset_name") %>%
      mutate(
        Issue_type                = "Automatic",
        Issue_noted_by_Lilly_Stats = "Consecutive visits should be >{day_lob/7} wk(s) and <{day_hib/7} wk(s)",
        PPD_Comment_or_resolution = "",
        Status                    = "New"
      )
    
    addWorksheet(wb, output_tab, tabColour = "#FFFF99")
    writeData(wb,
              sheet = output_tab,
              x     = final_issues)
  }
  
  invisible(NULL)
}



