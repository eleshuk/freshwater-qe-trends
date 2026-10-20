# Pipeline 04: split the model-ready data into lakes and rivers and transform the river covariates
# Run from the project root after run_03_joins.R
# Outputs: data/model/lakes_model_df.parquet, rivers_model_df.parquet, rivers_model_df_transformed.parquet
# and data/model/skew_check/rivers_transform_parameters.csv; covariate check plots in plots/covariate_checks

# Load the functions for this pipeline
source("R/0401_split_lakes_rivers.R")
source("R/0402_transform_rivers_data.R")
source("R/0403_check_covariates.R")
library(ggplot2)

# Paths
model_dir <- "data/model"

# 0401 Split lakes and rivers
split_lakes_rivers(model_dir)

# 0402 Transform the river covariates
transform_rivers_data(model_dir)

# 0403 Correlation, VIF and PCA checks behind the final covariate set (Fig. S3-S5)
covariate_checks <- check_covariates(
  input_file = file.path(model_dir, "rivers_model_df_transformed.parquet"),
  output_dir = "plots/covariate_checks"
)
