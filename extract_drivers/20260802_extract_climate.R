# Climate catchment extraction
# Extracts monthly climate statistics from E-OBS daily NetCDF
# grids per EU primary catchment (WSO1_ID).
#
# Source: E-OBS ensemble mean, 0.1deg regular grid, WGS84
#   v33.0e (1995-2010), v27.0e (2011-2022)
# Variables: tg (mean temp, C), tn (min temp, C), tx (max temp, C),
#            rr (rainfall, mm), hu (relative humidity, %)
#
# Monthly statistics produced per variable:
#   - mean (or sum for rr): central tendency / total
#   - max: absolute daily maximum within the month
#   - min: absolute daily minimum within the month
#
# Extraction: area-weighted mean of each monthly raster per catchment.
# NODATA: -9999 in source, read as NA by terra.
#         na.rm = TRUE during daily-to-monthly aggregation, so months
#         with partial daily coverage still produce a value. A pixel
#         with all-NA days in a month contributes NA to the extraction.
#
# Output: Driver_merge/Output/climate_catchment.parquet
#   Columns: WSO1_ID (int), variable (chr), stat (chr),
#            year (int), month (int), value (dbl)
#
# stat values:
#   tg/tn/tx/hu: "mean", "max", "min"
#   rr:          "sum", "max", "min"

library(terra)
library(sf)
library(exactextractr)
library(dplyr)
library(tidyr)
library(arrow)
library(ggplot2)
library(glue)

# --- Paths (working directory = Spatial_workflow_work/) ---
root <- "/Users/isa128539/Desktop/"
# root <- "/Users/eleshuk/Documents/ISA_class_materials/Thesis/"
# catchment_path <- paste0(root, "Spatial_workflow_work/Workflow_WFD/Data/test_data/Input_test_data.gpkg")
catchment_path <- paste0(root, "catchment_data/Input_data_filtered.gpkg")
# monthly_dir <- paste0(root, "data/climate/")
# output_path <- paste0(root, "data/climate/outputs/climate_catchment.parquet")
monthly_dir <- paste0(root, "drivers_data/climate/outputs")
output_path <- paste0(root, "drivers_data/climate/outputs/climate_catchment.parquet")


# Variables and their stat labels
# central_label distinguishes sum (rainfall) from mean (everything else)
variables <- tibble::tribble(
  ~var_code, ~central_label,
  "tg",      "mean",
  "tn",      "mean",
  "tx",      "mean",
  "rr",      "sum",
  "hu",      "mean"
)

# CDO stat file labels mapped to output stat names
# "mean" files contain monmean for temp/humidity, but for rainfall
# the central file also used monmean -- we relabel to "sum" via central_label
stat_files <- c("mean", "max", "min")

# --- Load catchments and reproject to WGS84 ---
message("Loading catchments...")
catchments <- sf::st_read(catchment_path, layer = "EU_primary_catchments", quiet = TRUE)
catchments_wgs84 <- sf::st_transform(catchments, 4326)
message(glue("  {nrow(catchments_wgs84)} catchments reprojected to EPSG:4326"))

wso_ids <- catchments_wgs84 |>
  sf::st_drop_geometry() |>
  dplyr::select(WSO1_ID)

# --- Extract one monthly raster file per catchment ---
extract_monthly_file <- function(nc_path, var_code, stat_label, catch, wso_ids) {
  r <- terra::rast(nc_path)

  # Parse year-month from the time dimension
  time_vals <- terra::time(r)
  ym_labels <- format(time_vals, "%Y_%m")
  names(r) <- ym_labels

  message(glue("  Extracting {var_code} {stat_label}: {terra::nlyr(r)} months..."))
  ex <- exactextractr::exact_extract(r, catch, fun = "mean", progress = TRUE)

  # Reshape to long format
  wso_ids |>
    dplyr::bind_cols(ex) |>
    tidyr::pivot_longer(
      cols = -WSO1_ID,
      names_to = "col_name",
      values_to = "value"
    ) |>
    dplyr::mutate(
      ym = sub("^mean\\.", "", col_name),
      variable = var_code,
      stat = stat_label,
      year = as.integer(substr(ym, 1, 4)),
      month = as.integer(substr(ym, 6, 7))
    ) |>
    dplyr::select(WSO1_ID, variable, stat, year, month, value)
}

# Write each var-stat combo to its own parquet file, then free memory - necessary due to large file size
for (v_i in seq_len(nrow(variables))) {
  v <- variables[v_i, ]
  
  for (sf in stat_files) {
    nc_path <- file.path(monthly_dir, glue("{v$var_code}_{sf}_monthly.nc"))
    
    if (!file.exists(nc_path)) {
      warning(glue("Not found: {nc_path} -- skipping"))
      next
    }
    
    out_label <- if (sf == "mean") v$central_label else sf
    chunk_path <- file.path(dirname(output_path),
                            glue("climate_{v$var_code}_{out_label}.parquet"))
    
    # Skip if already written
    if (file.exists(chunk_path)) {
      message(glue("  Exists: {basename(chunk_path)}"))
      next
    }
    
    result <- extract_monthly_file(nc_path, v$var_code, out_label,
                                   catchments_wgs84, wso_ids)
    message(glue("  -> {nrow(result)} rows, writing parquet..."))
    arrow::write_parquet(result, chunk_path)
    
    rm(result)
    gc()
  }
}
