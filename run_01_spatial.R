# Pipeline 01: prepare the WFD water bodies for the CCM2 catchment join
# Run from the project root after run_00_scrape_clean.R
# Output: data/spatial/ccm2_wfd_join/Input_data_filtered.gpkg

# Load the functions for this pipeline
source("R/0101_remove_cw_input_gpkg.R")

# Paths
ccm2_wfd_dir <- "data/spatial/ccm2_wfd_join"

# 0101 Remove coastal waters and water bodies without BQE data
remove_cw_input_gpkg(
  input_gpkg = file.path(ccm2_wfd_dir, "Input_data.gpkg"),
  swb_parquet = "data/WFD/parquet_tables_cleaned/swb_reference_filtered/part-0.parquet",
  output_gpkg = file.path(ccm2_wfd_dir, "Input_data_filtered.gpkg")
)

# 0102 Next, run the catchment join manually from the project root in a terminal:
#   python python/0102_assign_catchments.py
# It reads data/spatial/ccm2_wfd_join/Input_data_filtered.gpkg and writes data/spatial/Workflow_outputs.gpkg
# For an unattended run set WFD_EXECUTION_MODE=unattended and WFD_RUN_PROFILE=full first
