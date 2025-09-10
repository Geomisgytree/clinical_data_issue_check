# scripts/05_check_multi_site.R

library(dplyr)
library(purrr)
library(openxlsx)

# 2.4 Subjects in multiple sites
check_multi_site <- function(datasets_pool, wb, output_tab = NULL,
                             visit_info_df = NULL) {
  
  available_ds <- names(datasets_pool)
  
  multi_site_summary <- map_dfr(available_ds, function(ds_name) {
    df <- datasets_pool[[ds_name]]
    info_row <- visit_info_df %>% filter(dataset == ds_name)
    
    if (nrow(info_row) == 0) {
      warning("Dataset ", ds_name, " not found in visit_info_df. Skipped.")
      return(NULL)
    }
    
    subject_col <- info_row$subject_var
    site_col    <- info_row$site_var
    
    if (is.na(subject_col) || is.na(site_col)) {
      warning("Dataset ", ds_name, " missing subject/site column info. Skipped.")
      return(NULL)
    }
    
    df %>%
      filter(!is.na(SITEID), !is.na(SUBJECT_ID)) %>%
      group_by(SUBJECT_ID) %>%
      summarise(
        n_sites   = n_distinct(SITEID),
        site_list = paste(sort(unique(SITEID)), collapse = ", "),
        .groups   = "drop"
      ) %>%
      filter(n_sites > 1) %>%
      transmute(
        SUBJECT_ID,
        SITEID       = site_list,
        dataset_name = ds_name
      )
  })
  
  if (nrow(multi_site_summary) == 0) {
    message("No subjects in multiple sites found.")
    return(invisible(NULL))
  }
  
  issues <- multi_site_summary %>%
    mutate(
      Issue_type = "Automatic",
      Issue_noted_by_Lilly_Stats = "Subjects in multiple sites",
      PPD_Comment_or_resolution = "",
      Status = "New"
    )
  
  addWorksheet(wb, output_tab, tabColour = "#FFFF99")
  writeData(wb, sheet = output_tab, x = issues)
  
  invisible(NULL)
}
