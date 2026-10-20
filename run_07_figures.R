# Pipeline 07: thesis result figures and tables, and the data for the exploration maps
# Run from the project root after run_06_model_assessment.R
# Figures go to plots/model_results and plots/maps, tables to data/model/M4/m4_stage1_results_rivers/RW/tables

# Load packages and functions for this pipeline
library(ggplot2)
source("R/theme_thesis.R")
source("R/0600_model_helpers.R")
source("R/0701_plot_coefficients.R")
source("R/0702_plot_temporal_region.R")
source("R/0703_plot_variance_partitioning.R")
source("R/0005_join_bqe.R")
source("R/0301_create_spatial_joined.R")

# Paths
wfd_root <- "data/WFD"
results_dir <- "data/model/M4/m4_stage1_results_rivers/RW"
table_dir <- file.path(results_dir, "tables")
results <- load_bqe_results(results_dir)

# 0701 Driver coefficients (Fig. 10, Table S8)
coef_all <- run_coefficient_figure(results, table_dir)

# 0702 Observed and predicted status over time and by region (Fig. 7, 8, 9, 12); the slowest step
temporal <- run_temporal_region_figures(results, "data/model/rivers_model_df_transformed.parquet", table_dir)

# 0703 Variance partitioning (Fig. 11, Table S9)
variance_partitioning <- run_variance_partitioning(results, table_dir)

# 0704 Data for the exploration maps (Fig. 4, 5, 6, S8). Choose the filters to apply here; the defaults below
# keep the UK, coastal waters, angiosperms and macroalgae, as in the thesis maps
create_qe_ref_join_filtered(
  wfd_root, "QE1 - Biological quality elements",
  drop_qe = NULL,
  drop_categories = "TeW",
  drop_countries = NULL,
  file_stem = "joined_bqe_ref_maps",
  export_parquet = TRUE
)
purrr::walk(c("2010", "2016", "2022"), \(yr) {
  join_bqe_spatial_data(yr, wfd_root, joined_file = "joined_bqe_ref_maps_final.parquet", suffix = "maps")
})

# Then draw the maps from the project root in a terminal:
#   python python/0704_plot_bqe_maps.py
# To map the thesis dataset instead: python python/0704_plot_bqe_maps.py --joined-file joined_bqe_ref_w2010_final.parquet --spatial-suffix ""
