library(sf)
library(dplyr)
library(arrow)

# Inputs
input_gpkg <- "/Users/eleshuk/Documents/ISA_class_materials/Thesis/Spatial_workflow_work/Workflow_WFD/Data/Input_data.gpkg"
swb_parquet <- "/Users/eleshuk/Documents/ISA_class_materials/Thesis/data/WFD/parquet_tables_cleaned/swb_reference_filtered/part-0.parquet"
output_gpkg <- "/Users/eleshuk/Documents/ISA_class_materials/Thesis/Spatial_workflow_work/Workflow_WFD/Data/Input_data_filtered.gpkg"

st_layers(input_gpkg)

# Read and filter the reference data to exclude coastal waters
swb_filtered <- arrow::read_parquet(swb_parquet) |>
  dplyr::filter(surfaceWaterBodyCategory != "CW")

valid_codes <- unique(swb_filtered$euSurfaceWaterBodyCode)

# Start fresh
if (file.exists(output_gpkg)) file.remove(output_gpkg)

# Filter and write SurfaceWaterBody
swb <- sf::st_read(input_gpkg, layer = "SurfaceWaterBody", quiet = TRUE) |>
  dplyr::filter(WB_code %in% valid_codes)
sf::st_geometry(swb) <- "Shape"
sf::st_write(swb, output_gpkg, layer = "SurfaceWaterBody",
             quiet = TRUE, layer_options = "FID=OBJECTID")

# Filter and write SurfaceWaterBodyLine
swbl <- sf::st_read(input_gpkg, layer = "SurfaceWaterBodyLine", quiet = TRUE) |>
  dplyr::filter(WB_code %in% valid_codes)
sf::st_geometry(swbl) <- "Shape"
sf::st_write(swbl, output_gpkg, layer = "SurfaceWaterBodyLine",
             append = TRUE, quiet = TRUE, layer_options = "FID=OBJECTID")

# Write EU_primary_catchments as-is
catchments <- sf::st_read(input_gpkg, layer = "EU_primary_catchments", quiet = TRUE)
sf::st_geometry(catchments) <- "Shape"
sf::st_write(catchments, output_gpkg, layer = "EU_primary_catchments",
             append = TRUE, quiet = TRUE, layer_options = "FID=OBJECTID")

# Write Network_data as-is (non-spatial)
net_data <- sf::st_read(input_gpkg, layer = "Network_data", quiet = TRUE)
sf::st_write(net_data, output_gpkg, layer = "Network_data",
             append = TRUE, quiet = TRUE)

# Verify
st_layers(output_gpkg)
