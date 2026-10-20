# Pipeline 02: extract climate, nutrient, land cover, imperviousness and barrier drivers per CCM2 catchment
# Run from the project root after run_01_spatial.R; best run on a computer with > 50 GB RAM
# Raw driver data sits in data/drivers/<driver>/ and each output is written to data/drivers/<driver>/outputs/

# Load the functions for this pipeline
source("extract_drivers/0201_climate_preprocess.R")
source("extract_drivers/0202_extract_climate.R")
source("extract_drivers/0203_extract_nitrogen.R")
source("extract_drivers/0204_extract_phosphorus.R")
source("extract_drivers/0205_extract_land_cover.R")
source("extract_drivers/0206_extract_imperviousness.R")
source("extract_drivers/0207_extract_amber.R")

# Paths
drivers_root <- "data/drivers"
catchment_path <- "data/spatial/ccm2_wfd_join/Input_data_filtered.gpkg"

# Climate preprocessing method: "cdo" (needs CDO installed, fast) or "windows" (pure R, slow)
climate_method <- "cdo"

# 0201 Aggregate daily E-OBS climate to monthly files in data/drivers/climate/outputs
preprocess_climate(
  climate_base = file.path(drivers_root, "climate"),
  method = climate_method
)

# 0202 Extract monthly climate per catchment
extract_climate(
  catchment_path = catchment_path,
  monthly_dir = file.path(drivers_root, "climate", "outputs")
)
# On a machine with limited RAM (e.g. 16 GB), use the batched version instead:
# source("extract_drivers/0202b_extract_climate_batched.R")
# extract_climate_batched(
#   catchment_path = catchment_path,
#   monthly_dir = file.path(drivers_root, "climate", "outputs"),
#   batch_size = 100000
# )

# 0203 Nitrogen surplus
extract_nitrogen(
  n_dir = file.path(drivers_root, "nitrogen"),
  catchment_path = catchment_path,
  output_path = file.path(drivers_root, "nitrogen", "outputs", "nitrogen_catchment.parquet")
)

# 0204 Phosphorus surplus
extract_phosphorus(
  p_dir = file.path(drivers_root, "phosphorus"),
  catchment_path = catchment_path,
  output_path = file.path(drivers_root, "phosphorus", "outputs", "phosphorus_catchment.parquet")
)

# 0205 CORINE land cover
extract_land_cover(
  clc_base = file.path(drivers_root, "land_cover"),
  catchment_path = catchment_path,
  output_parquet = file.path(drivers_root, "land_cover", "outputs", "clc_catchment_area_by_class.parquet")
)

# 0206 Imperviousness density
extract_imperviousness(
  imd_dir = file.path(drivers_root, "imperviousness_density"),
  catchment_path = catchment_path,
  output_path = file.path(drivers_root, "imperviousness_density", "outputs", "imperviousness_catchment.parquet")
)

# 0207 AMBER barriers
extract_amber(
  amber_path = file.path(drivers_root, "dams", "atlas_AMBER.csv"),
  catchment_path = catchment_path,
  output_path = file.path(drivers_root, "dams", "outputs", "amber_catchment.parquet")
)
