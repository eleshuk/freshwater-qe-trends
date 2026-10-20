# Pipeline 06: assess the river CLMMs (thesis section 2.9; Tables S5-S7, Fig. S6-S7)
# Run from the project root after run_05_model.R; reads the saved fits, refits only fast clm and glm baselines
# Tables go to the results folder, figures to plots/model_assessment

# Load packages and functions for this pipeline
library(ggplot2)
source("R/theme_thesis.R")
source("R/0600_model_helpers.R")
source("R/0601_fit_assessment.R")
source("R/0602_po_checks.R")
source("R/0603_standardized_re_qq.R")

# Paths
results_dir <- "data/model/M4/m4_stage1_results_rivers/RW"

# 0601 AIC comparison, weighted kappa and observed vs predicted class shares
fit_assessment <- run_fit_assessment(results_dir)

# 0602 Parallel slopes check per cut point
po_check <- run_po_checks(results_dir)

# 0603 Standardized random-effect QQ plots
re_summary <- run_standardized_re_qq(results_dir)
