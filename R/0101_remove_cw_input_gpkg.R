# Remove coastal waters and water bodies without BQE data from Input_data.gpkg before the CCM2 catchment join
# Writes Input_data_filtered.gpkg, the input for python/0102_assign_catchments.py and the extract_drivers pipeline

# Filter the water body layers to valid codes and copy the catchment and network layers unchanged
remove_cw_input_gpkg <- function(input_gpkg, swb_parquet, output_gpkg) {
  # Valid codes are filtered reference water bodies that are not coastal waters
  valid_codes <- arrow::read_parquet(swb_parquet) |>
    dplyr::filter(surfaceWaterBodyCategory != "CW") |>
    dplyr::pull(euSurfaceWaterBodyCode) |>
    unique()

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

  # Return the layers written for a quick check
  sf::st_layers(output_gpkg)
}
