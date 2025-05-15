This repo contains data, analysis scripts, task scripts, and figures for the paper:

Morgan, O. P., & Casasanto, D. (2025). Frequency asymmetries in perception: The action asymmetry hypothesis. _Journal of Experimental Psychology: General_.

This repo is archived at: [https://osf.io/mc5gy](https://osf.io/mc5gy)

The experiments in this study were preregistered using AsPredicted:
- Experiment 1: https://aspredicted.org/GBB_3TD (#115434)
- Experiment 2: https://aspredicted.org/LDQ_TR8 (#132650)
- Experiment 3: https://aspredicted.org/3HW_56 (#133012)

# Contents
- [Data and analysis scripts](#data-and-analysis-scripts)
  - [Dependencies](#dependencies)
  - [Experiment 1](#experiment-1)
  - [Experiments 2 and 3](#experiments-2-and-3)
  - [Analyses combining data from Experiments 1 and 2](#analyses-combining-data-from-experiments-1-and-2)
- [Data dictionary](#data-dictionary)
  - [Long, per-trial data](#long-per-trial-data)
  - [Summary data](#summary-data)
- [Figures](#figures)
- [Experiment script and stimuli](#experiment-script-and-stimuli)
  - [Scripts (prefix: scripts\_)](#scripts-prefix-scripts_)
  - [Stimuli (prefix: stimuli\_)](#stimuli-prefix-stimuli_)
  - [Resources and instructions (prefixes: resources\_, inst\_)](#resources-and-instructions-prefixes-resources_-inst_)
  - [Consent form](#consent-form)

# Data and analysis scripts

Data, analysis scripts, and output (figures, tables, and html reports) are in the folder `analysis/`. This folder is the top level of an R Project indexed by the file `fava.Rproj`. If you are using RStudio, open the project using this file. All scripts use paths relative to the project root, using the function `here::here()`. Analysis scripts are found in `analysis/[Experiment folder]/analyses/`. The folder `[Experiment label]/lib/` contains supporting functions used by analysis scripts. `[Experiment folder]/figures/` contains figures. `[Experiment folder]/manual_cache` is used to cache models and other objects that can take a long time to create. Raw, intermediate, and processed data are found in `[Experiment folder]/data/`.

Analyses for each experiment are run using the script `run_analyses.R`. Scripts that must be run in order are prefixed with numbers (e.g., `experiment_1/1_process.Rmd` must be run before `experiment_1/2_exclude.Rmd`).

## Dependencies
Analyses were run using R 4.4.1. Dependencies are managed with renv: to install them, open the project and run `renv::restore()` when prompted.

On MacOS and linux, the R package `nloptr` (a dependency of `lme4`) depends on `cmake`.

## Experiment 1

Data and analysis scripts for Experiment 1 are found in `analysis/experiment_1/`. 

### Pilot data and power analysis
Rmarkdown scripts used to process and analyze the pilot data (a sample of 112 right handed recruits) are found in the folder `analyses/pilot/`:
- `1_process.Rmd`: Load and process raw data.
- `2_exclude.Rmd`: Apply and describe exclusions.
- `3_power.Rmd`: Power analysis.


### Full sample data and analyses
Rmarkdown scripts used to process and analyze the pilot data (a sample of 1008 recruits) are found in the folder `analyses/experiment_n1008/`:
- `1_process.Rmd`: Load and process raw data.
- `2_exclude.Rmd`: Apply and describe exclusions; report demographics
- `3_rt.Rmd`: Reaction time analyses.
- `3_acc.Rmd`: Accuracy analyses.
- The subfolder `components/` includes Rmarkdown files that are sourced in primary analysis scripts.


## Experiments 2 and 3

Data and analysis scripts for Experiment 2 and 3 are found in `analysis/experiment_2-3/analyses/`:
- `1_process.Rmd`: Load and process raw data (Exp. 2 and 3).
- `2_exclude.Rmd`: Apply and describe exclusions (Exp. 2 and 3).
- `3_rt.Rmd`: Reaction time analyses (Experiment 2).
- `3_acc.Rmd`: Accuracy analyses (Experiment 2).
- `3_dichotic.Rmd`: Dichotic listening analyses (Experiment 3)
- The subfolder `components/` includes Rmarkdown files that are sourced in primary analysis scripts.


## Analyses combining data from Experiments 1 and 2

Analyses of the combined data from experiments 1 and 2 are found in `analysis/experiments_1-2/analyses/`:
- `3_rt.Rmd`: Reaction time analyses (Exp. 1 and 2).
- `3_sex.Rmd`: Analyses on effect of sex (Exp. 1 and 2).


## Supplementary analyses: response hand congruence and task order
Analyses of response hand congruence and task order are found in `analysis/supplementary_analyses`:
- `supplementary_analyses.Rmd`

# Data dictionary
A copy of "the data" (the preprocessed data for every participant with at least one full experimental run) from each experiment is available in the `analysis/data/` directory. `data/[E1/E2-3]_aah_long.tsv` includes task data, with a row for every trial. `data/[E1/E2-3]_aah_summary.tsv` includes demographic and summary data, with a row for every subject. These files are copies of the files found in `experiment_1/data/proc_exp_n1008` and `experiments_2-3/data/proc_exp_n1450`.

## Long, per-trial data
The column names in `E1_aah_long.tsv` mean:
- *subject*: [string] subject ID (generated by prolific)
- *block_type*: [string: "practice", "main"]. Indicates "practice" trials or "main" experimental trials.  
- *block_response*: [string: "z", "slash"] Response key for "go" responses.
- *target_present*: [sting: "yes", "no"']
- *target*: [string: "absent", "square", "circle", "rectangle"]
- *level*: [string: "global", "local", "absent"] Was the target present at the local, or global level?
- *field*: [string: "LVF", "RVF", "absent] Was the target present in the left (LVF) or right visual field (RVF)?
- *correct* [boolean: 1, 0] Did the participant respond correctly? (button press if target was present, no button press if target was absent)
- *rt* [integer] reaction time, in milliseconds, from target onset.
- *first_block* [string: "z", "slash"] Response hand used in the first block.
- *ehi* [numeric: -100 to 100] Edinburgh Handedness Inventory score (Veale 4-item subscale). -100 means strongly left handed; 0 means ambidextrous; 100 means strongly right handed.
- *age* [numeric] Age in years.
- *country* [string] "Which country do you currently live in?"
- *sex* [string: "Male", "Female", "Not listed:"] "What is your sex?"
- *education* [numeric] "What is the highest degree or level of schooling you have completed? If currently enrolled, please list your highest degree completed so far." Converted to years of education.
- *race* [string: "Black or African American", "White","Asian", "Native Hawaiian or Other Pacific Islander", "American Indian or Alaska Native", "Some other race (please describe)"] "Please select your race. Check all that apply."
- *hispanic_ethnicity* [string: "Yes", "No"] "Are you hispanic or latino (a person of Cuban, Mexican, Puerto Rican, Cuban, South or Central American, or other Spanish culture or origin, regardless of race)?"
- *rt_overall* [numeric] Participant's median reaction time across all correct, target-present trials (ms).
- *duration_s* [numeric] Duration, in seconds, of the inquisit task for each participant.
- *task_experience_response* [string: "Yes", "No", "Not sure (please explain):"] "Have you ever done this task before?"
- *task_experience_other_response* [string] Text response if particant selected "Not sure (please explain)."
- *open_ended_feedback* [string] "(Optional) Is there anything you would like to share with the research team? Please write any feedback or information you think might be useful below."
- *exclude_many_gos* [Boolean: 1, 0] Exclude participant for pressing "go" in 78/80 or more trials.
- *exclude_low_acc* [Boolean: 1, 0] Exclude participant for accuracy below 60%.
- *exclude_low_rt* [Boolean: 1, 0] Exclude participant for fast response times (median 200ms or less).
- *exclude_high_rt* [Boolean: 1, 0] Exclude participant for slow response times (median 1500ms or greater).
- *sample* [string: "Left/mixedies", "Pilot righties", "New righties"]. Prolific data collection batch.
- *sample2* [string: "Left/mixedies", "All righties"] Prolific data collection batch (participants who reported to prolific that they were Left, Mixed, or Right handed).
- *prolific_handedness* [string: "Left", "Right", "Mixed"] Handedness participant reported to Prolific.
- *handedness* [string: "Left", "Right", "Mixed"] Handedness binned from EHI (Left <=40 < Mixed < 40 <= Right).
- *exclude_country* [Boolean: 1, 0] Exclude participants for living in a country other than the US (or UK, in experiment 2)
- *exclude_age* [Boolean: 1, 0] Exclude participants for being younger than 18 or older than 40.
- *exclude_done_before* [Boolean: 1, 0] Exclude participant for having done the task before.
- *exclude_no_ehi* [Boolean: 1, 0] Exclude participant for not completing the EHI.
- *exclude_ehi_mismatch* [Boolean: 1, 0] Exclude participant for having an EHI score that doesn't match the handedness they reported to Prolific.
- *exclude* [Boolean: 1, 0] Exclude participant. 

`E2-3_aah_long.tsv` has the following additional columns (used for dichotic listening analyses):
- *acc_slash* [numeric] Percentage accuracy for trials with the "slash" response key.
- *acc_z* [numeric] Percentage accuracy for trials with the "z" response key.
- *acc_absent* [numeric] Percentage accuracy for target-absent trials.
- *acc_present* [numeric] Percentage accuracy for target-present trials.
- *acc_global_LVF* [numeric] Percentage accuracy for trials with a global target present in the left visual field.
- *acc_global_RVF* [numeric] ... global target present in the right visual field.
- *acc_local_LVF* [numeric] ... local target present in the left visual field.
- *acc_local_RVF* [numeric] ... local target present in the right visual field.
- *rt_global_LVF* [numeric] Response time (ms) for trials with a global target present in the left visual field.
- *rt_global_RVF* [numeric] ... global target present in the right visual field.
- *rt_local_LVF* [numeric] ... local target present in the left visual field.
- *rt_local_RVF* [numeric] ... local target present in the right visual field.
- *headphonecheck_nCorrect* [integer: 1:6] Number of correct trials in the headphone check.
- *stereocheck_nCorrect* [integer: 1:6] Number of correct trials in the stereo audio check.
- *DL_right* [integer] Number of correct right-ear choices in the dichotic listening (DL) task.
- *DL_left* [integer] Number of correct left-ear choices.
- *DL_other* [integer] Number of incorrect choices.
- *DL_monoPctCorrect* [numeric] Percentage accuracy for mono, catch trials.
- *DL_dualPctCorrect* [numeric] Percentage accuracy for stereo, experimental trials.
- *DL_lat* [numeric] Dichotic listening laterality score. -100 means strongly left-eared; 100 means strongly right-eared.
- *DL_i1_clickhand* [string: "left", "right"] Reported response hand for the dichotic listening task.
- *exclude_visual* [Boolean: 1, 0] Exclude as visual outlier.
- *exclude_headphone_check* [Boolean: 1, 0] Exclude for failing headphone check.
- *exclude_stereo_check* [Boolean: 1, 0] Exclude for failing stereo audio check.
- *exclude_DL_mono_acc* [Boolean: 1, 0] Exclude for low accuracy on catch trials (below 75%).
- *exclude_DL_dual_acc* [Boolean: 1, 0] Exclude for low accuracy on experimental trials (below 60%).
- *exclude_dl* [Boolean: 1, 0] Exclude participant from dichotic listening task.
- *exclude_navon* [Boolean: 1, 0] Exclude participant from dichotic listening task.
- *exclude_any* [Boolean: 1, 0] Exclude participant from either task.

## Summary data
`[E1/E2-3]_aah_summary.tsv` contains a subset of the columns of `data/[E1/E2-3]_aah_long.tsv` (specifically, the columns with participant-level values). Refer to the data dictionary in the section "Long, per-trial data."


# Figures
The folder jepg_figures/ contains the final figures embedded in the manuscript:

1. `E1-E2_ehi`. Created by: `analysis/experiments_1-2/analyses/3_rt.Rmd`. Mutipanel figure composed in inkscape from the raw files saved in `analysis/experiments_1-2/figures/: demo_ehi_dots...png`.
1. `method_stimuli`. Created in inkscape.
1. `method_task_simpler`. Created in inkscape.
1. `E1-E2_cont`. Created by: `analysis/experiments_1-2/analyses/3_rt.Rmd`. Mutipanel figure composed in inkscape from the raw files saved in `analysis/experiments_1-2/figures/: rt_2_cor_line_E[n].png`.
1. `E1-E2_cat`. Created by: `analysis/experiments_1-2/analyses/3_rt.Rmd`. Edited in inkscape. Mutipanel figure composed in inkscape from the raw files saved in `analysis/experiments_1-2/figures/: rt_1_lmer_E[n]...png`.
1. `E1-E2_cat_sex`. Created by: `analysis/experiments_1-2/analyses/3_sex.Rmd`. Mutipanel figure composed in inkscape from the raw files saved in `analysis/experiments_1-2/figures/: rt_1_lmer_[F/M]...png`.
1. `E3_cor`. Created by: `analysis/experiments_2-3/analyses/3_dichotic.Rmd`. Mutipanel figure composed in inkscape from the raw files saved in `analyses/experiments_2-3/figures/: E3_cor.png; E3_cor_rightclick.png`.
1. `E3_density`. Created by: `analysis/experiments_2-3/analyses/3_dichotic.Rmd`. Mutipanel figure composed in inkscape from the raw files saved in `analyses/experiments_2-3/figures/: E3_density.png; E3_density_rightclick.png`.
1. `E3_cat`. Created by: Created by: `analysis/experiments_2-3/analyses/3_dichotic.Rmd`. Final figure composed in inkscape from the raw file saved as `analyses/experiments_2-3/figures/: freq_hand_2bins_big_model.png`.


# Experiment script and stimuli
The folder `inquisit_task/` contains experiment scripts, organized in subfolders names `Experiment_1/`, `Experiment_2/`, and `Experiment_3/`. `Experiment_1/` includes its bilateral heirarchical shapes task in the folder `circle_square_task/`; `Experiment_2/` includes its bilateral heirarchical shapes task in the folder `rectangle_square_task/`. `Experiment_3/` contains the dichotic listening task (`dichotic_listening_task/`), headphone check (`headphonecheck/`), and stereo audio check (`stereocheck/`).

Because of Inquisit Web's design, all scripts and resources for each task are in the same, top-level directory for that task, organized with filename prefixes:

## Scripts (prefix: scripts_)
`script_main.iqx` is the main executable. It calls the other files that have the prefix `script_`.

## Stimuli (prefix: stimuli_)
The heirarchical shape stimuli and mask have the prefix `stimuli_`. These svg files were created in inkscape by Mahek Majithia.

## Resources and instructions (prefixes: resources_, inst_)
Files with the prefixes `resources_` and `inst_` contain images and html shown as instructions, used in scripts. `style.css` contains css to style these instructions.

## Consent form
The consent form is the file `consent.html`.




