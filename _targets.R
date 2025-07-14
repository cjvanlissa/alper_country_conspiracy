# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(lavaan)
library(targets)
library(tarchetypes) # Load other packages as needed.
library(worcs)
library(lme4)
library(tuneRanger)
library(ranger)
library(glmnet)
library(tuneRanger)
library(mlr)
library(doParallel)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.
set.seed(812)

# Replace the target list below with your own:
list(
  # tar_target(
  #   name = datasets, # Item level data
  #   command = readRDS("sim_datasets.RData")
  # )
  # , tar_target(
  #   name = res_psychometrics,
  #   command = do_psychometrics(datasets)
  # )
  # ,
  tar_target(
    name = df,
    command = get_data()
    )
  , tar_target(
    name = df_withmiss,
    command = split_train_test(df, k = 10)
  )
  , tar_target(
    name = dat,
    command = impute_missings(df_withmiss)
  )
  , tar_target(
    name = res_brma,
    command = do_brma(dat, chains = 1, iter = 100)
  )
  , tar_target(
    name = res_metaforest,
    command = do_metaforest(dat)
  )
  , tar_target(
    name = res_metacart,
    command = do_metacart(dat)
  )
  , tar_target(
    name = analysis_results,
    command = eval_results(dat, models = list(BRMA = res_brma, MetaForest = res_metaforest, MetaCART = res_metacart))
  )
  , tarchetypes::tar_render(name = manuscript, path = "manuscript.rmd", output_file = "index.html", cue = tar_cue("always"))
)
