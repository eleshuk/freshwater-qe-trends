# Nitrogen surplus extraction per EU primary catchment
# Reads 16 method variants of the Batool et al. (2022) gridded N surplus dataset, computes the ensemble mean
# across methods, filters to 2004-2019, and extracts area-weighted mean N surplus per catchment polygon
#
# Source: Batool, M., Sarrazin, F.J., Attinger, S., Basu, N.B., Van Meter, K. & Kumar, R. (2022).
#   Long-Term Annual Soil Nitrogen Surplus Across Europe 1850-2019.
#   Helmholtz Centre for Environmental Research - UFZ. https://www.globewq.info
#
# Output: nitrogen_catchment.parquet with WSO1_ID, year, n_surplus_kg_ha

# Ensemble mean N surplus per catchment and year
extract_nitrogen <- function(n_dir, catchment_path, output_path, target_years = 2004:2019) {
  # Load catchments and reproject to WGS84 to match the N surplus grid (EPSG:4326, ~5 arcmin)
  message("Loading catchment polygons...")
  catchments_wgs84 <- sf::st_read(catchment_path, layer = "EU_primary_catchments", quiet = TRUE) |>
    sf::st_transform(4326)
  message(glue::glue("{nrow(catchments_wgs84)} catchments loaded and reprojected to EPSG:4326"))

  # Build file list for all 16 methods
  method_files <- file.path(
    n_dir,
    glue::glue("N_sur_total_kg_ha_grid_area_1850-2019_method_{1:16}_v1.0.nc")
  )
  stopifnot(all(file.exists(method_files)))

  # Identify target year indices from the time axis
  r_probe <- terra::rast(method_files[1], subds = "Nsurplus")
  layer_years <- as.integer(format(terra::time(r_probe), "%Y"))
  target_idx <- which(layer_years %in% target_years)
  stopifnot(length(target_idx) == length(target_years))
  message(glue::glue("Target years {min(target_years)}-{max(target_years)}: layers {min(target_idx)}-{max(target_idx)}"))

  # Accumulate a running sum one method at a time, then divide by the method count
  n_methods <- length(method_files)
  ensemble_sum <- NULL
  for (i in seq_along(method_files)) {
    message(glue::glue("  Adding method {i}/{n_methods}..."))
    r <- terra::rast(method_files[i], subds = "Nsurplus")
    r_subset <- r[[target_idx]]
    ensemble_sum <- if (is.null(ensemble_sum)) r_subset else ensemble_sum + r_subset
    rm(r, r_subset)
  }
  ensemble <- ensemble_sum / n_methods
  rm(ensemble_sum)
  names(ensemble) <- paste0("Y", target_years)

  # Extract area-weighted mean per catchment and reshape to long format
  message("Extracting area-weighted mean N surplus per catchment...")
  extracted <- exactextractr::exact_extract(ensemble, catchments_wgs84, fun = "mean")

  nitrogen_df <- catchments_wgs84 |>
    sf::st_drop_geometry() |>
    dplyr::select(WSO1_ID) |>
    dplyr::bind_cols(extracted) |>
    tidyr::pivot_longer(
      cols = -WSO1_ID,
      names_to = "year_col",
      values_to = "n_surplus_kg_ha"
    ) |>
    dplyr::mutate(year = as.integer(sub("mean\\.Y", "", year_col))) |>
    dplyr::select(WSO1_ID, year, n_surplus_kg_ha)

  # Summary
  message(glue::glue("Output: {nrow(nitrogen_df)} rows, {dplyr::n_distinct(nitrogen_df$WSO1_ID)} catchments, {dplyr::n_distinct(nitrogen_df$year)} years"))
  print(summary(nitrogen_df$n_surplus_kg_ha))
  message(glue::glue("NA fraction: {round(mean(is.na(nitrogen_df$n_surplus_kg_ha)) * 100, 2)}%"))

  # Write output
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  arrow::write_parquet(nitrogen_df, output_path)
  message(glue::glue("Written to {output_path}"))
  invisible(nitrogen_df)
}
