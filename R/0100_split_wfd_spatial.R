# Split the WISE WFD reference spatial GeoPackages into one GeoParquet per cycle and geometry type
# Writes {spatial_root}/surface_water_bodies/{year}_spatial_data_swb.parquet (polygons) and
# {spatial_root}/rivers/{year}_spatial_data_rivers.parquet (lines), read by R/0301_create_spatial_joined.R

# Write the polygon and line layers of one cycle's GeoPackage to GeoParquet
split_wfd_spatial <- function(year, spatial_dir = "data/WFD/spatial_data",
                              spatial_root = "data/WFD/parquet_tables_spatial") {
  # Find this cycle's GeoPackage, e.g. 4326_250_k_wise-wfd2016_p_2016-2025_v01_r09.gpkg
  gpkg <- list.files(spatial_dir, pattern = glue::glue("wise-wfd{year}_.*\\.gpkg$"), full.names = TRUE)
  if (length(gpkg) != 1) {
    stop("Expected one GeoPackage for ", year, " in ", spatial_dir, ", found ", length(gpkg))
  }

  # Output paths, one folder per geometry type
  swb_path <- glue::glue("{spatial_root}/surface_water_bodies/{year}_spatial_data_swb.parquet")
  rivers_path <- glue::glue("{spatial_root}/rivers/{year}_spatial_data_rivers.parquet")
  dir.create(dirname(swb_path), showWarnings = FALSE, recursive = TRUE)
  dir.create(dirname(rivers_path), showWarnings = FALSE, recursive = TRUE)

  # Polygons: lakes, transitional and coastal waters, and rivers mapped as areas
  cat(year, ": SurfaceWaterBody ->", swb_path, "\n")
  sf::st_read(gpkg, layer = "SurfaceWaterBody", quiet = TRUE) |>
    sfarrow::st_write_parquet(swb_path)

  # Lines: rivers
  cat(year, ": SurfaceWaterBodyLine ->", rivers_path, "\n")
  sf::st_read(gpkg, layer = "SurfaceWaterBodyLine", quiet = TRUE) |>
    sfarrow::st_write_parquet(rivers_path)

  invisible(c(swb = swb_path, rivers = rivers_path))
}
