# AMBER Barrier Atlas: per-catchment barrier counts
# Point-in-polygon spatial join of AMBER barriers to EU primary catchments (CCM2)

library(sf)
library(dplyr)
library(arrow)

# ------ Input paths ------
# root <- "/Users/eleshuk/Documents/ISA_class_materials/Thesis/"
root <- "/Users/isa128539/Desktop/"
amber_path <- paste0(root, "drivers_data/dams/atlas_AMBER.csv")
catchment_path <- paste0(root, "catchment_data/Input_data_filtered.gpkg")
output_path <- paste0(root, "drivers_data/dams/outputs/amber_catchment.parquet")

# ------ Read AMBER barriers ------
# Semicolon-delimited, WGS84 coordinates
amber_raw <- read.csv2(amber_path, stringsAsFactors = FALSE)

cat("AMBER records read:", nrow(amber_raw), "\n")

# Convert coordinate columns to numeric (they may read as character)
amber_raw <- amber_raw |>
  dplyr::mutate(
    Longitude_WGS84 = as.numeric(Longitude_WGS84),
    Latitude_WGS84 = as.numeric(Latitude_WGS84)
  )

# Drop records with missing coordinates
n_no_coords <- sum(is.na(amber_raw$Longitude_WGS84) | is.na(amber_raw$Latitude_WGS84))
cat("Records dropped (missing coordinates):", n_no_coords, "\n")

amber_valid <- amber_raw |>
  dplyr::filter(!is.na(Longitude_WGS84), !is.na(Latitude_WGS84))

# Build sf point layer and reproject to EPSG:3035
amber_sf <- amber_valid |>
  sf::st_as_sf(coords = c("Longitude_WGS84", "Latitude_WGS84"), crs = 4326) |>
  sf::st_transform(3035)

cat("AMBER points after CRS transform:", nrow(amber_sf), "\n")

# ------ Read catchments ------

catchments <- sf::st_read(catchment_path, layer = "EU_primary_catchments")
cat("Catchments loaded:", nrow(catchments), "\n")

# ------ Point-in-polygon spatial join ------
# Assign each barrier its enclosing catchment's WSO1_ID
amber_catch <- sf::st_join(amber_sf, catchments[, "WSO1_ID"], join = sf::st_within)

# Log barriers that fell outside any catchment
n_outside <- sum(is.na(amber_catch$WSO1_ID))
cat("Barriers outside catchment coverage:", n_outside, "\n")

amber_catch <- amber_catch |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(WSO1_ID))

# ------ Aggregate per catchment ------

# Total barrier count per catchment
total_count <- amber_catch |>
  dplyr::group_by(WSO1_ID) |>
  dplyr::summarise(n_barriers_total = dplyr::n(), .groups = "drop")

# Count by barrier type per catchment (long format)
type_count <- amber_catch |>
  dplyr::group_by(WSO1_ID, Type) |>
  dplyr::summarise(n_barriers = dplyr::n(), .groups = "drop")

# Join total onto the per-type table
amber_summary <- type_count |>
  dplyr::left_join(total_count, by = "WSO1_ID")

cat("Output rows:", nrow(amber_summary), "\n")
cat("Catchments with at least one barrier:", dplyr::n_distinct(amber_summary$WSO1_ID), "\n")

# ------ Write output ------

arrow::write_parquet(amber_summary, output_path)
cat("Written to:", output_path, "\n")
