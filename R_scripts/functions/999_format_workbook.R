
format_issue_log_sheets <- function(wb) {
  sheets <- names(wb)[-1]  # Exclude READ_ME
  
  # Define styles
  header_style    <- createStyle(halign = "center", textDecoration = "bold", fontSize = 12, fgFill = "#D9D9D9")
  content_style   <- createStyle(halign = "center", wrapText = TRUE)
  blue_header     <- createStyle(halign = "center", textDecoration = "bold", fontSize = 12, fgFill = "#DDEBF7")
  bold_readme     <- createStyle(textDecoration = "bold")
  date_style      <- createStyle(numFmt = "yyyy-mm-dd", halign = "center")
  closed_style    <- createStyle(fgFill = "#C6EFCE", textDecoration = "bold", halign = "center")
  open_style      <- createStyle(fgFill = "#FFA500", textDecoration = "bold", halign = "center")
  recurred_style  <- createStyle(fgFill = "#FF6347", textDecoration = "bold", halign = "center")
  highlight_cols  <- c("Issue_type", "Issue_noted_by_Lilly_Stats", "PPD_Comment_or_resolution", "Status")
  bold_rows       <- c(1, 3, 11)
  
  for (sheet in sheets) {
    print(sheet)
    df <- readWorkbook(wb, sheet = sheet, detectDates = TRUE)
    df[sapply(df, inherits, "Date")] <- lapply(df[sapply(df, inherits, "Date")], as.character)
    
    writeData(wb, sheet = sheet, x = df, startRow = 1, colNames = TRUE, withFilter = FALSE)
    
    # Auto width
    est_width <- pmax(nchar(colnames(df)), sapply(df, function(col) max(nchar(as.character(col)), na.rm = TRUE)))
    setColWidths(wb, sheet = sheet, cols = 1:ncol(df), widths = pmin(est_width + 2, 50))
    
    # Apply styles
    addStyle(wb, sheet, header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
    addStyle(wb, sheet, content_style, rows = 2:(nrow(df) + 1), cols = 1:ncol(df), gridExpand = TRUE)
    
    wrap_cols <- which(sapply(df, function(col) any(nchar(as.character(col)) > 30)))
    if (length(wrap_cols)) {
      wrap_style <- createStyle(wrapText = TRUE, halign = "left")
      addStyle(wb, sheet, wrap_style, rows = 2:(nrow(df) + 1), cols = wrap_cols, gridExpand = TRUE)
    }
    
    freezePane(wb, sheet, firstRow = TRUE)
    addFilter(wb, sheet, rows = 1, cols = 1:ncol(df))
    
    # Highlight
    highlight_idx <- which(tolower(colnames(df)) %in% tolower(highlight_cols))
    if (length(highlight_idx)) {
      addStyle(wb, sheet, blue_header, rows = 1, cols = highlight_idx, gridExpand = TRUE)
    }
    
    # Status color
    status_idx <- which(colnames(df) == "Status") ## match the actual status variable
    if (length(status_idx)) {
      status_values <- df[[status_idx]]
      addStyle(wb, sheet, closed_style, rows = which(str_detect(tolower(status_values), "closed|permanent")) + 1, cols = status_idx)
      addStyle(wb, sheet, open_style,   rows = which(str_detect(tolower(status_values), "open")) + 1, cols = status_idx)
      addStyle(wb, sheet, recurred_style, rows = which(str_to_lower(status_values) == "recurred") + 1, cols = status_idx)
    }
    
    # Format date columns
    date_idx <- which(tolower(colnames(df)) %in% c("first_detected_date", "last_checked_date"))
    if (length(date_idx)) {
      addStyle(wb, sheet, date_style, rows = 2:(nrow(df) + 1), cols = date_idx, gridExpand = TRUE)
    }
  }
  
  ##### Order all the tabs
  all_sheets <- names(wb)
  # Keep only tabs that actually exist
  main_tabs_declared <- c(
    "READ_ME", "ED_NOT_FOUND", "Visit_discrepancy_issues", "EX_SV_TRTDSPN_DATE_MISMATCH",
    "SV1001_DS6001", "subject_in_multiple_sites", "visits_gap_issue", "ED_and_v801_check"
  )
  main_tabs_present <- intersect(main_tabs_declared, all_sheets)
  ## other dataset tabs
  other_tabs <- setdiff(all_sheets, main_tabs_present)
  
  # Sort other tabs alphabetically (or apply your own custom logic)
  sorted_other_tabs <- sort(other_tabs)
  
  # Final desired order contains every sheet exactly once
  new_sheet_order_names <- c(main_tabs_present, sorted_other_tabs)
  
  # Map to indices; guaranteed no NA because we intersected earlier
  wb$sheetOrder <- match(new_sheet_order_names, all_sheets)
  
  # Defensive: ensure it's a proper 1..n permutation (optional assert)
  stopifnot(!any(is.na(wb$sheetOrder)),
            length(unique(wb$sheetOrder)) == length(all_sheets))
  
  # Style READ_ME if present
  if ("READ_ME" %in% all_sheets) {
    ## set appropriate width for the readme tab
    df_readme <- readWorkbook(wb, sheet = "READ_ME")
    est_width <- sapply(df_readme, function(col) max(nchar(as.character(col)), na.rm = TRUE))
    est_width <- pmin(est_width + 2, 80)
    
    setColWidths(wb, sheet = "READ_ME", cols = 1:2, widths = est_width[1:2])
    ## add bold rows
    addStyle(wb, "READ_ME", bold_readme, rows = bold_rows, cols = c(1,2), gridExpand = TRUE)
    
  }
}








