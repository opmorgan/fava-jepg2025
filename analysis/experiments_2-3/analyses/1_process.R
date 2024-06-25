# ----------------------------------------------------------------------------#
## Setup
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# ----------------------------------------------------------------------------#
## Lib
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(cli) # For printing error messages
library(glue) # To make writing error message strings easier


EXPERIMENT_DIR <- "experiments_2-3"
source(here::here(EXPERIMENT_DIR, "lib", "load_process", "load_process.R"))

# ----------------------------------------------------------------------------#
## Config
reprocess_all <- F

if (reprocess_all == T) {
  reprocess_navon_data <- TRUE
  reprocess_dichotic_data <- TRUE
  reprocess_ehi_data <- TRUE
  reprocess_demographics_data <- TRUE
  reprocess_end_data <- TRUE
  reprocess_clickhand_data <- TRUE
  
  
  resummarize_navon_data <- TRUE
  resummarize_dichotic_data <- TRUE
  resummarize_ehi_data <- TRUE
  resummarize_demographics_data <- TRUE
  resummarize_end_data <- TRUE
  resummarize_clickhand_data <- TRUE
  
  
  recombine_navon_data <- TRUE
  recombine_summary_data <- TRUE
  
} else if (reprocess_all == F) {
  reprocess_navon_data <- F
  reprocess_dichotic_data <- F
  reprocess_ehi_data <- F
  reprocess_demographics_data <- F
  reprocess_end_data <- F
  reprocess_clickhand_data <- F
  
  resummarize_navon_data <- F
  resummarize_dichotic_data <- F
  resummarize_ehi_data <- F
  resummarize_demographics_data <- F
  resummarize_end_data <- F
  resummarize_clickhand_data <- F
  
  recombine_navon_data <- F
  recombine_summary_data <- F
}

data_dir <- here::here(EXPERIMENT_DIR, "data")
input_dir <- here::here(data_dir, "input_exp_n1450")
proc_dir <- here::here(data_dir, "proc_exp_n1450")

# ----------------------------------------------------------------------------#
## Load and check quality of individual-level data
### Load and process task data
## If data have been processed already, set to FALSE
if (reprocess_navon_data == TRUE) {
  process_raw(input_dir, proc_dir, data_type = "navon")
}

if (reprocess_dichotic_data == TRUE) {
  process_raw(input_dir, proc_dir, data_type = "dichotic")
}

## Load and process survey data
if (reprocess_ehi_data == TRUE) {
  process_raw(input_dir, proc_dir, data_type = "ehi")
}

if (reprocess_demographics_data == TRUE) {
  process_raw(input_dir, proc_dir, data_type = "demographics")
}

if (reprocess_end_data == TRUE) {
  process_raw(input_dir, proc_dir, data_type = "end")
}

if (reprocess_clickhand_data == TRUE) {
  process_raw(input_dir, proc_dir, data_type = "clickhand")
}

# ----------------------------------------------------------------------------#
## Find calculated variables, save summary data
## Load processed data
ind_input_dir <- here(proc_dir, "individual")
summary_output_dir <- here(proc_dir, "summary")

## Create summary row for each subject
## TODO: Add overall median RT to summary spreadsheet
if (resummarize_navon_data == TRUE) {
  ## Create summarized navon data (saves to tsv as a side effect)
  ## Calculate any exclusions based on navon data
  navon_summary <-
    load_and_summarize_proc(ind_input_dir, summary_output_dir,
                            data_type = "navon")
} else {
  navon_summary <- read_tsv(here(summary_output_dir, "summary_navon.tsv"))
}

if (resummarize_dichotic_data == TRUE) {
  ## Create summarized navon data (saves to tsv as a side effect)
  ## Calculate any exclusions based on navon data
  dichotic_summary <-
    load_and_summarize_proc(ind_input_dir, summary_output_dir,
                            data_type = "dichotic")
} else {
  dichotic_summary <- read_tsv(here(summary_output_dir, "summary_dichotic.tsv"))
}

if (resummarize_ehi_data == TRUE) {
  ## Create summarized navon data (saves to tsv as a side effect)
  ehi_summary <-
    load_and_summarize_proc(ind_input_dir, summary_output_dir,
                            data_type = "ehi")
} else {
  ehi_summary <- read_tsv(here(summary_output_dir, "summary_ehi.tsv"))
}

if (resummarize_demographics_data == TRUE) {
  ## Create summarized demographics data (saves to tsv as a side effect)
  demo_summary <-
    load_and_summarize_proc(ind_input_dir, summary_output_dir,
                            data_type = "demographics")
} else {
  demo_summary <- read_tsv(here(summary_output_dir, "summary_demographics.tsv"))
}

if (resummarize_end_data == TRUE) {
  ## Create summarized end question data (saves to tsv as a side effect)
  end_summary <-
    load_and_summarize_proc(ind_input_dir, summary_output_dir,
                            data_type = "end")
} else {
  end_summary <- read_tsv(here(summary_output_dir, "summary_end.tsv"))
}

if (resummarize_clickhand_data == TRUE) {
  ## Create summarized end question data (saves to tsv as a side effect)
  clickhand_summary <-
    load_and_summarize_proc(ind_input_dir, summary_output_dir,
                            data_type = "clickhand")
} else {
  clickhand_summary <- read_tsv(here(summary_output_dir, "summary_clickhand.tsv"))
}

# ----------------------------------------------------------------------------#
# Create combined summary table (with task and survey data)
if (resummarize_navon_data == TRUE
    | resummarize_dichotic_data == TRUE
    | resummarize_ehi_data == TRUE
    | resummarize_demographics_data == TRUE
    | resummarize_end_data == TRUE
    | resummarize_clickhand_data == TRUE) {
## Combine summaries into one big table, with a wide row for each subject.
summary <- navon_summary |>
  left_join(dichotic_summary) |>
  left_join(ehi_summary) |>
  left_join(demo_summary) |>
  left_join(end_summary) |> 
  left_join(clickhand_summary)

write_tsv(summary, here::here(summary_output_dir, "summary.tsv"))
}

# ----------------------------------------------------------------------------#
#Create combined trial-per-row table (with task and survey data, and summary stats)
ind_input_dir <- here(proc_dir, "individual")
combined_output_dir <- here(proc_dir, "combined")

if (recombine_navon_data == TRUE) {
  navon_combined <-
    load_and_combine_proc(ind_input_dir, combined_output_dir, data_type = "navon")
}

if (recombine_summary_data == TRUE) {
  summary <- load_summary(proc_dir)
}

if (recombine_navon_data == TRUE
    | recombine_summary_data == TRUE) {
  ## Combine all processed data into one long table
  combined <- navon_combined |>
    left_join(summary)
  save_path <- here::here(combined_output_dir, "combined_all.tsv")
  cli::cli_alert_info("{.strong Saving combined navon, survey, and summary data to:}")
  cli::cli_bullets(c(" " = glue("{save_path}")))
  write_tsv(combined, save_path)
}
