# Phosphorus surplus extraction per EU primary catchment
# Ensemble mean across all P_sur method variants, 2004-2019
#
# Source: Batool et al. (2021), UFZ/GlobeWQ
# Variable: Psurplus (kg ha-1 yr-1), 5 arcmin grid, WGS84
# Methods: 48 files (methods 1-48), each 170 annual layers (1850-2019)
# Grid: 1020 lon x 552 lat

library(terra)
library(sf)
library(exactextractr)
library(dplyr)
library(tidyr)
library(arrow)
library(glue)

# --- Paths ---
root <- "/Users/isa128539/Desktop/"
p_dir <- paste0(root, "drivers_data/phosphorus")
catchment_path <- paste0(root, "catchment_data/Input_data_filtered.gpkg")
output_path <- paste0(root, "drivers_data/phosphorus/outputs/phosphorus_catchment.parquet")

# Discover all P surplus method files
p_files <- sort(list.files(p_dir, pattern = "^P_sur_total_kg_ha_grid_area_1850-2019_method_.*\\.nc$", full.names = TRUE))
n_methods <- length(p_files)
message(glue("{n_methods} phosphorus surplus method files found"))

# --- Read spatial metadata from terra ---
r1 <- rast(p_files[1])
lon_vals <- xFromCol(r1, 1:ncol(r1))
lat_vals <- yFromRow(r1, 1:nrow(r1))
half_res <- mean(diff(lon_vals)) / 2
half_res <- 5 / 60 / 2  # 2.5 arcmin = 0.04167°
p_ext <- ext(
  -24.95833 - half_res,   # xmin (left cell edge)
  59.95833 + half_res,   # xmax (right cell edge)
  26.04167 - half_res,   # ymin (bottom cell edge)
  71.95833 + half_res    # ymax (top cell edge)
)
message(sprintf("P grid extent: %.2f to %.2f lon, %.2f to %.2f lat",
                p_ext[1], p_ext[2], p_ext[3], p_ext[4]))

# lat_descending <- lat_vals[1] > lat_vals[length(lat_vals)]
lat_descending <- FALSE

# --- Identify target year layers ---
time_vals <- time(r1)
years_all <- as.integer(format(time_vals, "%Y"))
target_years <- 2004:2019
target_idx <- which(years_all %in% target_years)
message(glue("Target layers: {min(target_idx)} to {max(target_idx)} (1-indexed), {length(target_idx)} years"))

# --- Compute ensemble mean per year ---
# Accumulate running sum and non-NA count across methods.
# All methods share the same spatial mask, but the count approach
# is robust to methods with differing coverage.
sum_rast <- NULL
count_rast <- NULL

for (m in seq_along(p_files)) {
  message(glue("Reading method file {m}/{n_methods}: {basename(p_files[m])}"))
  
  method_layers <- rast(p_files[m], lyrs = target_idx)
  ext(method_layers) <- p_ext
  crs(method_layers) <- "EPSG:4326"
  
  if (!lat_descending) method_layers <- flip(method_layers, direction = "vertical")
  method_layers <- ifel(method_layers != method_layers, NA, method_layers)
  
  # Binary mask: 1 where data exists, 0 where NA
  not_na <- !is.na(method_layers)
  
  # Replace NA with 0 so addition doesn't propagate NA
  method_zero <- subst(method_layers, NA, 0)
  
  if (is.null(sum_rast)) {
    sum_rast <- method_zero
    count_rast <- not_na
  } else {
    sum_rast <- sum_rast + method_zero
    count_rast <- count_rast + not_na
  }
}

# Divide by per-pixel method count
ensemble_rast <- sum_rast / count_rast

# Convert NaN (0/0 at pixels where no method had data) to proper NA
# so exact_extract skips them instead of propagating NaN
ensemble_rast <- subst(ensemble_rast, NaN, NA)

names(ensemble_rast) <- paste0("Y", target_years)
message(glue("Ensemble raster: {nlyr(ensemble_rast)} layers, {ncol(ensemble_rast)} x {nrow(ensemble_rast)} pixels"))

rm(sum_rast, count_rast); gc()

# --- Load and reproject catchments ---
catchments <- st_read(catchment_path, layer = "EU_primary_catchments", quiet = TRUE)
message(glue("{nrow(catchments)} catchments loaded"))

catchments_4326 <- st_transform(catchments, 4326)

# --- Extract area-weighted mean per catchment ---
message("Extracting phosphorus surplus per catchment...")
extracted <- exact_extract(ensemble_rast, catchments_4326, fun = "mean",
                           append_cols = "WSO1_ID")

# --- Reshape to long format ---
result <- extracted |>
  tidyr::pivot_longer(
    cols = starts_with("mean.Y"),
    names_to = "year",
    values_to = "p_surplus_kg_ha"
  ) |>
  dplyr::mutate(year = as.integer(gsub("mean\\.Y", "", year)))

# Coverage summary
n_valid <- sum(!is.na(result$p_surplus_kg_ha))
n_total <- nrow(result)
message(glue("Output: {n_total} rows, {length(unique(result$WSO1_ID))} catchments x {length(unique(result$year))} years"))
message(glue("Non-NA values: {n_valid} ({round(100 * n_valid / n_total, 1)}%)"))

# --- Write output ---
dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
write_parquet(result, output_path)
message(glue("Written to {output_path}"))


# library(arrow)
# p <- read_parquet("/Users/isa128539/Desktop/drivers_data/phosphorus/outputs/phosphorus_catchment.parquet")
# cat("Rows:", nrow(p), "\n")
# cat("Non-NA:", sum(!is.na(p$p_surplus_kg_ha)), "/", nrow(p),
#     sprintf("(%.1f%%)\n", 100 * mean(!is.na(p$p_surplus_kg_ha))))
# summary(p$p_surplus_kg_ha)

