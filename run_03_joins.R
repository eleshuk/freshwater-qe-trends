# Pipeline 03: join BQE data to spatial data, drivers, biogeographical regions and river basins
# Run from the project root after run_00_scrape_clean.R, run_01_spatial.R (including the Python step)
# and run_02_extract_drivers.R
# Output: data/model/model_ready_biogeo_rb.parquet

# Load the functions for this pipeline
source("R/0301_create_spatial_joined.R")
source("R/0302_join_no_geom.R")
source("R/0303_build_driver_matrix.R")
source("R/0304_merge_biogeography.R")
source("R/0305_merge_rbs.R")

# Paths
wfd_root <- "data/WFD"
model_dir <- "data/model"
workflow_gpkg <- "data/spatial/Workflow_outputs.gpkg"
dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)

# 0301 Join BQE data to the water body geometries, one file per cycle
purrr::walk(c("2010", "2016", "2022"), \(yr) join_bqe_spatial_data(yr, wfd_root))

# 0302 Drop geometry and keep water bodies in the BQE table
join_bqe_nogeo(wfd_root)

# 0303 Build the pressure matrix, merge drivers and write model_ready.parquet (set use_cache = FALSE to recompute)
model_df <- run_driver_pipeline(
  wfd_root = wfd_root,
  drivers_root = "data/drivers",
  workflow_gpkg = workflow_gpkg,
  cache_dir = "data/driver_cache",
  model_dir = model_dir,
  use_cache = TRUE
)

# 0304 Add biogeographical regions
merge_biogeography(
  model_ready_path = file.path(model_dir, "model_ready.parquet"),
  biogeo_path = "data/biogeographical_regions/BiogeoRegions2016.shp",
  wb_drainage_path = "data/biogeographical_regions/WB_drainage_areas.gpkg",
  wb_coords_path = file.path(wfd_root, "parquet_tables_joined", "bqe_ref_nogeo.parquet"),
  output_path = file.path(model_dir, "model_ready_biogeo.parquet")
)

# 0305 Add CCM2 river basins and drop rows without climate drivers
merge_rbs(
  workflow_gpkg = workflow_gpkg,
  gdb_path = "data/spatial/CCM2_stuff.gdb",
  input_path = file.path(model_dir, "model_ready_biogeo.parquet"),
  output_path = file.path(model_dir, "model_ready_biogeo_rb.parquet")
)
