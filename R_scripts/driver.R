
##### KIAC issue tracker 27AUG2025 #####
## Intended for KIAC: In-house study

rm(list = ls())
library(dplyr)
library(openxlsx)
library(haven)
library(stringr)
library(purrr)
library(tidyr)

# --- Important: Change all the directories ---
setwd("~/lillyce/qa/ly3541860/j3k_mc_kiac/intrm1/programs/component_modules/data issue/") ## set working directory
MODULE_PATH <- "/lillyce/qa/ly3541860/j3k_mc_kiac/intrm1/programs/component_modules/data issue/" ## data issue parent folder
SPEC_PATH <- paste0(MODULE_PATH, "documentation/KIAC_raw_data_check_issue_spec.xlsx") ## issue spec location
DATA_PATH <- "/lillyce/qa/ly3541860/j3k_mc_kiac/prelock/data/raw/shared/" # where raw data have been located
STUDY_NAME <- "J3K-MC-KIAC" ## Please enter your full study name
MASTER_PATH  <- paste0(MODULE_PATH, "output/KIAC_Stats_issues_log_cumulative.xlsx") ## final excel output file

# --- Confirm: All the directories have been changed ---


##### 0. Load functions #####
all_check <- read.xlsx(SPEC_PATH, sheet = "ALL_DATASETS") %>% 
  filter(is.na(REMOVE))
simple_check <- read.xlsx(SPEC_PATH, sheet = "SINGLE_DATASETS") %>% 
  filter(is.na(REMOVE))


##### 1. Read data and setup #####
source(paste0(MODULE_PATH, "programs/functions/000_setup.R"))
data_list <- load_dataset(DATA_PATH, ## raw data path
                          pattern = "\\.sas7bdat$",
                          exclude_prefixes = c("cdms"), ## exclude datasets that starting with sys, cdms
                          exclude_names = c("record_count", "rtsm_sbjct_data", "sd", 
                                            "mhpresp1001") ## exclude certain datasets
)

dataset_names <- names(data_list) ## a list including all the dataset names

## load all the functions
files <- list.files(file.path(MODULE_PATH, "programs/functions"), 
                    pattern = "\\.R$", full.names = TRUE)
walk(files, source)


##### 2. Read spec and get all the parameters #####

## step 1: Preprocessing all the datasets 
preprocess_spec <- read.xlsx(SPEC_PATH, sheet = "PREPROCESSING")

## Preprocess the raw data and output data info
# -- data_info: include SUBJECT, SITEID, column for visit date and visit column
data_info <- preprocess_data(dataset_names, preprocess_spec) # please check data_info to see if subject ID, SITE ID, visit date, visit label were correct

datasets_pool <- list()

for (nm in dataset_names) {
  if (exists(nm, envir = .GlobalEnv)) {
    datasets_pool[[nm]] <- get(nm, envir = .GlobalEnv)
  } else {
    warning("Dataset ", nm, " does not exist in global environment.")
  }
}

##### 3. Issue check #####
wb <- createWorkbook()
wb <- create_readme_sheet(wb) ## Create ReadMe data sheet

## Detect required issue check
for (i in seq_len(nrow(all_check))) {
  fun <- all_check$FUNCTION_NAME[i] # Function name
  output_tab  <- all_check$OUTPUT_TAB[i] # Output tab name
  ds_req_str <- all_check$DATASET_REQUIRED[i] # Required datasets - string
  other_ds <- strsplit(all_check$DATASET_REQUIRED, ",")[[1]] %>% trimws()
  
  if (is.na(ds_req_str) || ds_req_str == "" || tolower(ds_req_str) == "all") {
    required_ds <- NULL
  } else {
    required_ds <- strsplit(ds_req_str, ",")[[1]] %>% trimws()
  }
  
  
  message(paste0("🔎 Running check: ", fun, " → Output tab: ", output_tab))
  
  ## If the function exists
  if (!exists(fun)) {
    warning(paste("❌ Function" , fun, "not found! Skipped."))
    next
  }
  
  ## If the required dataset(s) exists
  if (!is.null(required_ds)) {
    if (!all(required_ds %in% dataset_names)) {
      missing_ds <- setdiff(required_ds, dataset_names)
      warning("Function ", fun, " lacks dataset(s): ", paste(missing_ds, collapse = ", "))
      next
    }
  }
  
  ## parameters for each function
  param_list <- list(
    datasets_pool = datasets_pool,
    wb = wb,
    visit_info_df = data_info$visit_info_df,
    output_tab = output_tab
  )
  
  param_str <- all_check$PARAMS[i]

  if (!is.na(param_str) && nchar(param_str) > 0) {
    # Split on commas **outside** parentheses
    params <- strsplit(param_str, ",(?=(?![^()]*\\)))", perl = TRUE)[[1]] %>% trimws()
    
    for (p in params) {
      kv <- strsplit(p, "=", fixed = TRUE)[[1]]
      if (length(kv) < 2) next
      
      key <- trimws(kv[1])
      value_expr <- paste(kv[-1], collapse = "=") %>% trimws()
      
      # If the value is column name from all_check
      if (value_expr %in% names(all_check)) {
        value <- all_check[[value_expr]][i]
        # Try splitting into vector if needed
        if (is.character(value) && grepl(",", value)) {
          value <- strsplit(value, ",")[[1]] %>% trimws()
        }
      } else {
        # Try to parse expressions like c(1,2,3)
        value <- tryCatch(eval(parse(text = value_expr)), error = function(e) value_expr)
      }
      
      param_list[[key]] <- value
    }
  }
  

  do.call(fun, param_list)
}

##### 4. Single dataset #####
created_sheets <- names(wb)
sheet_last_row <- list()

## loop through each unique dataset
for (ds_name in unique(simple_check$DATASET)) {
  
  ## if dataset name doesn't exist, skip
  if (!(ds_name %in% names(datasets_pool))) {
    warning(paste0("Dataset ", ds_name, " not found in datasets_pool."))
    next
  }
  
  ## get data and checkpoints obs
  ds_data <- datasets_pool[[ds_name]]
  checks <- simple_check %>% filter(DATASET == ds_name)
  check_results <- list()
  
  for (j in seq_len(nrow(checks))) {
    custom_code <- checks$CUSTOMIZED_CODE[j]
    issue_text  <- checks$CHECKPOINT[j]
    output_tab  <- checks$OUTPUT_TAB[j]
    
    custom_code <- gsub("[\r\n]", " ", checks$CUSTOMIZED_CODE[j]) 
    custom_code <- gsub("[[:space:]]+$", "", custom_code)         
    custom_code <- gsub("^[[:space:]]+", "", custom_code)         
    
    eval_env <- new.env(parent = globalenv())
    eval_env$ds_data <- ds_data
    list2env(datasets_pool, envir = eval_env)

    filtered_data <- tryCatch(
      eval(parse(text = custom_code), envir = eval_env),
      error = function(e) {
        warning(paste("Error in custom code for", ds_name, ":", conditionMessage(e)))
        return(NULL)
      }
    )
    
    if (nrow(filtered_data) > 0) {
      processed_data <- raw_process(filtered_data, issue_text)
      check_results[[length(check_results) + 1]] <- processed_data
    }
  }
  
  ## append the results into the original sheet
  if (length(check_results) > 0) {
    combined_data <- bind_rows(check_results)
    combined_data[] <- lapply(combined_data, as.character)
    
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
    
    writeData(wb, sheet = ds_name, x = combined_data, startRow = start_row, colNames = write_header)

  }
}


##### 5. Read and update log #####
# combined_issues <- readWorkbook(wb, sheet = "Issue Log")
sheet_keys <- list()
wb <- update_issue_log(wb, MASTER_PATH)


##### 6. Format/Style adjustment #####
format_issue_log_sheets(wb)

saveWorkbook(wb, MASTER_PATH, overwrite = TRUE)

# source("/lillyce/prd/general/rums/macro_library/ut_rlogcheck.R")
# 
# 
# ##### log check #####
# 
# #------ CREATE THE LOG FILE ------
# rout <- file(paste0(MODULE_PATH, "logs/tracker.log"), open = "wt", encoding = "UTF-8")
# 
# # Sink output to log file:
# sink(rout, append = T)
# 
# # Sink errors/warnings to log file:
# sink(rout, append = T, type = "message")
# 
# 
# print(sessionInfo())
# 
# # Sink output back to console:
# sink()
# 
# # Sink errors/warnings back to console:
# sink(type = "message")
# 
# # Close the log file
# close(rout)
# 
# # Check the log for issues
# logcheck <- ut_rlogcheck(logfile = paste0(MODULE_PATH, "logs/tracker.log"), 
#                          outfile = paste0(MODULE_PATH, "logs/tracker.lst"))


