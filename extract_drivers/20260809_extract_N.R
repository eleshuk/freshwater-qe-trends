# Nitrogen surplus extraction per EU primary catchment
#
# Reads 16 method variants of the Batool et al. (2021) gridded N surplus
# dataset, computes the ensemble mean across methods, filters to 2004-2019,
# and extracts area-weighted mean N surplus per catchment polygon.
#
# Source: Batool, M., Sarrazin, F.J., Attinger, S. & Kumar, R. (2021).
#   Long-Term Annual Soil Nitrogen Surplus Across Europe 1850-2019.
#   Helmholtz Centre for Environmental Research - UFZ.
#   https://www.globewq.info
#
# Input:  16 NetCDF files (methods 1-16) in ../data/nitrogen/
#         EU_primary_catchments from Workflow_WFD/Data/Input_data.gpkg
# Output: /Users/eleshuk/Documents/ISA_class_materials/Thesis/data/nitrogen/outputs/nitrogen_catchment.parquet
#
# Assumes working directory is Spatial_workflow_work/
library(terra)
library(sf)
library(exactextractr)
library(dplyr)
library(tidyr)
library(arrow)
library(glue)
library(ggplot2)

# --- Paths ---
# root <- "/Users/eleshuk/Documents/ISA_class_materials/Thesis/"
root <- "/Users/isa128539/Desktop/"
# n_dir <- paste0(root, "data/nitrogen")
n_dir <- paste0(root, "drivers_data/nitrogen")
catchment_path <- paste0(root, "catchment_data/Input_data_filtered.gpkg")
# output_path <- paste0(root, "data/nitrogen/outputs/nitrogen_catchment.parquet")
output_path <- paste0(root, "drivers_data/nitrogen/outputs/nitrogen_catchment.parquet")

# Years to extract (matching WFD reporting period, capped at data availability)
target_years <- 2004:2019

# --- Load catchments ---
cat("Loading catchment polygons...\n")
catchments <- sf::st_read(catchment_path, layer = "EU_primary_catchments", quiet = TRUE)

# Reproject to WGS84 to match the N surplus grid (EPSG:4326, ~5 arcmin)
catchments_wgs84 <- sf::st_transform(catchments, 4326)
cat(glue("{nrow(catchments_wgs84)} catchments loaded and reprojected to EPSG:4326"), "\n")

# --- Build file list for all 16 methods ---
method_files <- file.path(
  n_dir,
  glue("N_sur_total_kg_ha_grid_area_1850-2019_method_{1:16}_v1.0.nc")
)
stopifnot(all(file.exists(method_files)))
cat("All 16 method files found\n")

# --- Identify target year indices from the time axis ---
r_probe <- terra::rast(method_files[1], subds = "Nsurplus")
layer_times <- terra::time(r_probe)
layer_years <- as.integer(format(layer_times, "%Y"))
target_idx <- which(layer_years %in% target_years)
stopifnot(length(target_idx) == length(target_years))
cat(glue("Target years {min(target_years)}-{max(target_years)}: layers {min(target_idx)}-{max(target_idx)}"), "\n")

# --- Compute ensemble mean incrementally (one method at a time) ---
cat("Computing ensemble mean across 16 methods (incremental)...\n")
n_methods <- length(method_files)

# Accumulate a running sum, then divide at the end
ensemble_sum <- NULL

for (i in seq_along(method_files)) {
  cat(glue("  Adding method {i}/{n_methods}..."), "\n")
  r <- terra::rast(method_files[i], subds = "Nsurplus")
  r_subset <- r[[target_idx]]
  
  if (is.null(ensemble_sum)) {
    ensemble_sum <- r_subset
  } else {
    ensemble_sum <- ensemble_sum + r_subset
  }
  
  # Free the per-method raster
  rm(r, r_subset)
}

# Divide accumulated sum by method count
ensemble <- ensemble_sum / n_methods
rm(ensemble_sum)
names(ensemble) <- paste0("Y", target_years)
cat(glue("Ensemble mean raster: {terra::nlyr(ensemble)} layers"), "\n")

# --- Extract area-weighted mean per catchment ---
cat("Extracting area-weighted mean N surplus per catchment...\n")
extracted <- exactextractr::exact_extract(ensemble, catchments_wgs84, fun = "mean")

# Build long-format output table
nitrogen_df <- catchments_wgs84 |>
  sf::st_drop_geometry() |>
  dplyr::select(WSO1_ID) |>
  dplyr::bind_cols(extracted) |>
  tidyr::pivot_longer(
    cols = -WSO1_ID,
    names_to = "year_col",
    values_to = "n_surplus_kg_ha"
  ) |>
  dplyr::mutate(
    year = as.integer(sub("mean\\.Y", "", year_col))
  ) |>
  dplyr::select(WSO1_ID, year, n_surplus_kg_ha)

cat(glue("Output: {nrow(nitrogen_df)} rows, {dplyr::n_distinct(nitrogen_df$WSO1_ID)} catchments, {dplyr::n_distinct(nitrogen_df$year)} years"), "\n")

# --- Summary ---
cat("\nSummary of extracted N surplus (kg/ha/yr):\n")
print(summary(nitrogen_df$n_surplus_kg_ha))
na_frac <- mean(is.na(nitrogen_df$n_surplus_kg_ha))
cat(glue("NA fraction: {round(na_frac * 100, 2)}%"), "\n")

# --- Write output ---
dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
arrow::write_parquet(nitrogen_df, output_path)
cat(glue("Written to {output_path}"), "\n")
# # Nitrogen surplus extraction per EU primary catchment
# #
# # Reads 16 method variants of the Batool et al. (2021) gridded N surplus
# # dataset, computes the ensemble mean across methods, filters to 2004-2019,
# # and extracts area-weighted mean N surplus per catchment polygon.
# #
# # Source: Batool, M., Sarrazin, F.J., Attinger, S. & Kumar, R. (2021).
# #   Long-Term Annual Soil Nitrogen Surplus Across Europe 1850-2019.
# #   Helmholtz Centre for Environmental Research - UFZ.
# #   https://www.globewq.info
# #
# # Input:  16 NetCDF files (methods 1-16) in ../data/nitrogen/
# #         EU_primary_catchments from Workflow_WFD/Data/Input_data.gpkg
# # Output: /Users/eleshuk/Documents/ISA_class_materials/Thesis/data/nitrogen/outputs/nitrogen_catchment.parquet
# #
# # Assumes working directory is Spatial_workflow_work/
# 
# library(terra)
# library(sf)
# library(exactextractr)
# library(dplyr)
# library(tidyr)
# library(arrow)
# library(glue)
# library(ggplot2)
# 
# # --- Paths ---
# root <- "/Users/eleshuk/Documents/ISA_class_materials/Thesis/"
# n_dir <- paste0(root, "data/nitrogen")
# catchment_path <- paste0(root, "Spatial_workflow_work/Workflow_WFD/Data/test_data/Input_test_data.gpkg")
# output_path <- paste0(root, "data/nitrogen/outputs/nitrogen_catchment.parquet")
# 
# # Years to extract (matching WFD reporting period, capped at data availability)
# target_years <- 2004:2019
# 
# # --- Load catchments ---
# cat("Loading catchment polygons...\n")
# catchments <- sf::st_read(catchment_path, layer = "EU_primary_catchments", quiet = TRUE)
# 
# # Reproject to WGS84 to match the N surplus grid (EPSG:4326, ~5 arcmin)
# catchments_wgs84 <- sf::st_transform(catchments, 4326)
# cat(glue("{nrow(catchments_wgs84)} catchments loaded and reprojected to EPSG:4326"), "\n")
# 
# # --- Build file list for all 16 methods ---
# method_files <- file.path(
#   n_dir,
#   glue("N_sur_total_kg_ha_grid_area_1850-2019_method_{1:16}_v1.0.nc")
# )
# stopifnot(all(file.exists(method_files)))
# cat("All 16 method files found\n")
# 
# # --- Identify target year indices from the time axis ---
# r_probe <- terra::rast(method_files[1], subds = "Nsurplus")
# layer_times <- terra::time(r_probe)
# layer_years <- as.integer(format(layer_times, "%Y"))
# target_idx <- which(layer_years %in% target_years)
# stopifnot(length(target_idx) == length(target_years))
# cat(glue("Target years {min(target_years)}-{max(target_years)}: layers {min(target_idx)}-{max(target_idx)}"), "\n")
# 
# # --- Compute ensemble mean across 16 methods for each target year ---
# cat("Computing ensemble mean across 16 methods...\n")
# 
# # Load target-year subset from each method
# method_stacks <- lapply(seq_along(method_files), function(i) {
#   cat(glue("  Loading method {i}..."), "\n")
#   r <- terra::rast(method_files[i], subds = "Nsurplus")
#   r[[target_idx]]
# })
# 
# # Per-year mean across all 16 methods
# ensemble <- terra::rast(lapply(seq_along(target_idx), function(ly) {
#   layer_set <- terra::rast(lapply(method_stacks, function(s) s[[ly]]))
#   terra::app(layer_set, fun = "mean", na.rm = TRUE)
# }))
# 
# names(ensemble) <- paste0("Y", target_years)
# cat(glue("Ensemble mean raster: {terra::nlyr(ensemble)} layers"), "\n")
# 
# # --- Extract area-weighted mean per catchment ---
# cat("Extracting area-weighted mean N surplus per catchment...\n")
# extracted <- exactextractr::exact_extract(ensemble, catchments_wgs84, fun = "mean")
# 
# # Build long-format output table
# nitrogen_df <- catchments_wgs84 |>
#   sf::st_drop_geometry() |>
#   dplyr::select(WSO1_ID) |>
#   dplyr::bind_cols(extracted) |>
#   tidyr::pivot_longer(
#     cols = -WSO1_ID,
#     names_to = "year_col",
#     values_to = "n_surplus_kg_ha"
#   ) |>
#   dplyr::mutate(
#     year = as.integer(sub("mean\\.Y", "", year_col))
#   ) |>
#   dplyr::select(WSO1_ID, year, n_surplus_kg_ha)
# 
# cat(glue("Output: {nrow(nitrogen_df)} rows, {dplyr::n_distinct(nitrogen_df$WSO1_ID)} catchments, {dplyr::n_distinct(nitrogen_df$year)} years"), "\n")
# 
# # --- Summary ---
# cat("\nSummary of extracted N surplus (kg/ha/yr):\n")
# print(summary(nitrogen_df$n_surplus_kg_ha))
# 
# na_frac <- mean(is.na(nitrogen_df$n_surplus_kg_ha))
# cat(glue("NA fraction: {round(na_frac * 100, 2)}%"), "\n")
# 
# # --- Write output ---
# dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
# arrow::write_parquet(nitrogen_df, output_path)
# cat(glue("Written to {output_path}"), "\n")
# 
# 
# 
