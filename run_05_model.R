# Pipeline 05: fit the per-BQE ordinal CLMMs for rivers (thesis sections 2.8-2.9)
# Run from the project root after run_04_model_prep.R; takes about 45 minutes for all five BQEs on a MacBook Air
# BQEs that already have a saved fit are skipped, set refit = TRUE to refit them
# Output: data/model/M4/m4_stage1_results_rivers/RW (fits, coefficients, linear predictors, log, summary)

# Load the functions for this pipeline
source("R/0501_fit_clmm.R")

# 0501 Fit the five river BQE models with water body, basin and country random intercepts
fit_stage1(
  model_dir = "data/model",
  country = TRUE,
  bqe_only = river_bqes,
  refit = FALSE
)
