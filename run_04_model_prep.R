# Pipeline 04: split the model-ready data into lakes and rivers and transform the river covariates
# Run from the project root after run_03_joins.R
# Outputs: data/model/lakes_model_df.parquet, rivers_model_df.parquet, rivers_model_df_transformed.parquet
# and data/model/skew_check/rivers_transform_parameters.csv

# Load the functions for this pipeline
source("R/0401_split_lakes_rivers.R")
source("R/0402_transform_rivers_data.R")

# Paths
model_dir <- "data/model"

# 0401 Split lakes and rivers
split_lakes_rivers(model_dir)

# 0402 Transform the river covariates
transform_rivers_data(model_dir)
