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
targets::tar_option_set(priority = 1)

# Replace the target list below with your own:
list(
  tar_target(
    name = datasets, # Item level data
    command = readRDS("sim_datasets.RData")
  )
  , tar_target(
    name = res_psychometrics,
    command = do_psychometrics(datasets)
  )
  , tar_target(
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
  # , tar_target(
  #   name = features,
  #   command = get_features(dat = dat, flnm = "./data/codebook.xlsx")
  # )
  # , tar_target(
  #   name = dat_features,
  #   command = select_features(dat, features)
  # )
  # , tar_target(
  #   name = res_lasso,
  #   command = lapply(dat_features, do_lasso)
  # )
  # , tar_target(
  #   name = res_ranger,
  #   command = lapply(dat_features, do_ranger)
  # )
  # , tar_target(
  #   name = res_tree,
  #   command = lapply(dat_features, do_tree)
  # )
  # , tar_target(
  #   name = res_nn,
  #   command = do_nn(dat_features, epochs = 10) # Change to 500 for real data
  # )
  # , tar_target(
  #   name = analysis_results,
  #   command = eval_results(dat, models = list(lasso = res_lasso, ranger = res_ranger, tree = res_tree, nn = res_nn))
  # )
  # , tarchetypes::tar_render(manuscript, "manuscript.rmd", cue = tar_cue("always"), priority = 0.5)
  # , tar_file (
  #   name = create_index,
  #   command = { file.rename("manuscript.html", "index.html"); return("index.html")},
  #   cue = tar_cue("always"),
  #   priority = 0
  #   )
)
