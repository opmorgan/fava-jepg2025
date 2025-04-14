# ----------------------------------------------------------------------------#
## Setup
# ----------------------------------------------------------------------------#
library(here)
here::i_am("run_analyses.R")

## Function to render Rmd given path (relative to project root)
render_rmd <- function(rmd_relative_path) {
  rmarkdown::render(paste0(here(), rmd_relative_path))
}

# ----------------------------------------------------------------------------#
## Experiment 1 analyses
# ----------------------------------------------------------------------------#

## Pilot data and power analysis
#### 1_process.R: Load and process raw data.
source(paste0(here(),'/experiment_1/analyses/pilot/1_process.R'))

#### 2_exclude.Rmd: Apply and describe exclusions.
render_rmd('/experiment_1/analyses/pilot/2_exclude.Rmd')

#### 3_power.Rmd: Power analyses.
render_rmd('/experiment_1/analyses/pilot/3_power.Rmd')


## Full sample data and analyses
#### 1_process.R: Load and process raw data.
source(paste0(here(),'/experiment_1/analyses/experiment_n1008/1_process.R'))

#### 2_exclude.Rmd: Apply and describe exclusions; report demographics
render_rmd('/experiment_1/analyses/experiment_n1008/2_exclude.Rmd')

#### 3_rt.Rmd: Reaction time analyses.
render_rmd('/experiment_1/analyses/experiment_n1008/3_rt.Rmd')

#### 3_acc.Rmd: Accuracy analyses.
render_rmd('/experiment_1/analyses/experiment_n1008/3_acc.Rmd')


# ----------------------------------------------------------------------------#
## Experiments 2 and 3
# ----------------------------------------------------------------------------#
#### 1_process.Rmd: Load and process raw data (Exp. 2 and 3).
source(paste0(here(),'/experiments_2-3/analyses/1_process.R'))

#### 2_exclude.Rmd: Apply and describe exclusions (Exp. 2 and 3).
render_rmd('/experiments_2-3/analyses/2_exclude.Rmd')

#### 3_rt.Rmd: Reaction time analyses (Experiment 2).
render_rmd('/experiments_2-3/analyses/3_rt.Rmd')

#### 3_acc.Rmd: Accuracy analyses (Experiment 2).
render_rmd('/experiments_2-3/analyses/3_acc.Rmd')

#### 3_dichotic.Rmd: Dichotic listening analyses (Experiments 2-3)
render_rmd('/experiments_2-3/analyses/3_dichotic.Rmd')


# ----------------------------------------------------------------------------#
## Analyses combining data from Experiments 1 and 2
# ----------------------------------------------------------------------------#
#### 3_rt.Rmd: Reaction time analyses (Exp. 1 and 2).
render_rmd('/experiments_1-2/analyses/3_rt.Rmd')

#### 3_sex.Rmd: Analyses on effect of sex (Exp. 1 and 2).
render_rmd('/experiments_1-2/analyses/3_sex.Rmd')


# ----------------------------------------------------------------------------#
#### Supplementary analyses of response hand congruence and task order
render_rmd('/supplementary_analyses/supplementary_analyses.Rmd')





