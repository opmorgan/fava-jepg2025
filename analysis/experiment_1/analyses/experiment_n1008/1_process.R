
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

EXPERIMENT_DIR <- "experiment_1"
source(here::here(EXPERIMENT_DIR, "lib", "load_process", "load_process.R"))

# ----------------------------------------------------------------------------#
# Config
reprocess_all <- F

if (reprocess_all == T) {
  reprocess_task_data <- TRUE
  reprocess_ehi_data <- TRUE
  reprocess_demographics_data <- TRUE
  reprocess_end_data <- TRUE
  
  resummarize_task_data <- TRUE
  resummarize_ehi_data <- TRUE
  resummarize_demographics_data <- TRUE
  resummarize_end_data <- TRUE
  
  recombine_task_data <- TRUE
  recombine_summary_data <- TRUE
  
} else if (reprocess_all == F) {
  reprocess_task_data <- FALSE
  reprocess_ehi_data <- FALSE
  reprocess_demographics_data <- FALSE
  reprocess_end_data <- FALSE
  
  resummarize_task_data <- FALSE
  resummarize_ehi_data <- FALSE
  resummarize_demographics_data <- FALSE
  resummarize_end_data <- FALSE
  
  recombine_task_data <- FALSE
  recombine_summary_data <- FALSE
}

data_dir <- here::here(EXPERIMENT_DIR, "data")
input_dir <- here::here(data_dir, "input_exp_n1008")
proc_dir <- here::here(data_dir, "proc_exp_n1008")

# ----------------------------------------------------------------------------#
## Load and check quality of individual-level data
### Load and process task data
## If data have been processed already, set to FALSE
if (reprocess_task_data == TRUE) {
  process_raw(input_dir, proc_dir, data_type = "task")
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

# ----------------------------------------------------------------------------#
## Find calculated variables, save summary data
## Load processed data
ind_input_dir <- here(proc_dir, "individual")
summary_output_dir <- here(proc_dir, "summary")

## Create summary row for each subject
if (resummarize_task_data == TRUE) {
  ## Create summarized task data (saves to tsv as a side effect)
  ## Calculate any exclusions based on task data
  task_summary <-
    load_and_summarize_proc(ind_input_dir, summary_output_dir,
                            data_type = "task")
}

if (resummarize_ehi_data == TRUE) {
  ## Create summarized task data (saves to tsv as a side effect)
  ehi_summary <-
    load_and_summarize_proc(ind_input_dir, summary_output_dir,
                            data_type = "ehi")
}

if (resummarize_demographics_data == TRUE) {
  ## Create summarized demographics data (saves to tsv as a side effect)
  demo_summary <-
    load_and_summarize_proc(ind_input_dir, summary_output_dir,
                            data_type = "demographics")
}

if (resummarize_end_data == TRUE) {
  ## Create summarized end question data (saves to tsv as a side effect)
  end_summary <-
    load_and_summarize_proc(ind_input_dir, summary_output_dir,
                            data_type = "end")
}


# ----------------------------------------------------------------------------#
# Create combined summary table (with task and survey data)
if (resummarize_task_data == TRUE
    | resummarize_ehi_data == TRUE
    | resummarize_demographics_data == TRUE
    | resummarize_end_data == TRUE) {
  ## Combine summaries into one big table, with a wide row for each subject.
  summary <- task_summary |>
    left_join(ehi_summary) |>
    left_join(demo_summary) |>
    left_join(end_summary)
  
  write_tsv(summary, here::here(summary_output_dir, "summary.tsv"))
}

# ----------------------------------------------------------------------------#
#Create combined trial-per-row table (with task and survey data, and summary stats)
ind_input_dir <- here(proc_dir, "individual")
combined_output_dir <- here(proc_dir, "combined")

if (recombine_task_data == TRUE) {
  task_combined <-
    load_and_combine_proc(ind_input_dir, combined_output_dir, data_type = "task")
}

if (recombine_summary_data == TRUE) {
  summary <- load_summary(proc_dir)
}

if (recombine_task_data == TRUE
    | recombine_summary_data == TRUE) {
  ## Combine all processed data into one long table
  combined <- task_combined |>
    left_join(summary)
  save_path <- here::here(combined_output_dir, "combined_all.tsv")
  cli::cli_alert_info("{.strong Saving combined task, survey, and summary data to:}")
  cli::cli_bullets(c(" " = glue("{save_path}")))
  write_tsv(combined, save_path)
}


