# Phosphorus surplus extraction per EU primary catchment
# Ensemble mean across all P_sur method variants, 2004-2019
#
# Source: Batool, Sarrazin & Kumar (2025), Earth System Science Data 17, 881-916
# Variable: Psurplus (kg ha-1 yr-1), 5 arcmin grid, WGS84
# Methods: 48 files (methods 1-48), each 170 annual layers (1850-2019), grid 1020 lon x 552 lat
#
# Output: phosphorus_catchment.parquet with WSO1_ID, year, p_surplus_kg_ha

# Ensemble mean P surplus per catchment and year
extract_phosphorus <- function(p_dir, catchment_path, output_path, target_years = 2004:2019) {
  # Discover all P surplus method files
  p_files <- sort(list.files(p_dir, pattern = "^P_sur_total_kg_ha_grid_area_1850-2019_method_.*\\.nc$", full.names = TRUE))
  n_methods <- length(p_files)
  message(glue::glue("{n_methods} phosphorus surplus method files found"))

  # The files lack a usable extent, so set it from the known cell centres (2.5 arcmin half cell)
  half_res <- 5 / 60 / 2
  p_ext <- terra::ext(
    -24.95833 - half_res,
    59.95833 + half_res,
    26.04167 - half_res,
    71.95833 + half_res
  )
  lat_descending <- FALSE

  # Identify target year layers
  years_all <- as.integer(format(terra::time(terra::rast(p_files[1])), "%Y"))
  target_idx <- which(years_all %in% target_years)
  message(glue::glue("Target layers: {min(target_idx)} to {max(target_idx)} (1-indexed), {length(target_idx)} years"))

  # Accumulate a running sum and non-NA count across methods, robust to differing coverage
  sum_rast <- NULL
  count_rast <- NULL
  for (m in seq_along(p_files)) {
    message(glue::glue("Reading method file {m}/{n_methods}: {basename(p_files[m])}"))

    method_layers <- terra::rast(p_files[m], lyrs = target_idx)
    terra::ext(method_layers) <- p_ext
    terra::crs(method_layers) <- "EPSG:4326"

    if (!lat_descending) method_layers <- terra::flip(method_layers, direction = "vertical")
    method_layers <- terra::ifel(method_layers != method_layers, NA, method_layers)

    # Binary mask of data, and NA replaced with 0 so addition doesn't propagate NA
    not_na <- !is.na(method_layers)
    method_zero <- terra::subst(method_layers, NA, 0)

    if (is.null(sum_rast)) {
      sum_rast <- method_zero
      count_rast <- not_na
    } else {
      sum_rast <- sum_rast + method_zero
      count_rast <- count_rast + not_na
    }
  }

  # Divide by per-pixel method count, turning 0/0 NaN into NA so exact_extract skips them
  ensemble_rast <- terra::subst(sum_rast / count_rast, NaN, NA)
  names(ensemble_rast) <- paste0("Y", target_years)
  rm(sum_rast, count_rast)
  gc()

  # Load catchments, reproject and extract the area-weighted mean
  catchments_4326 <- sf::st_read(catchment_path, layer = "EU_primary_catchments", quiet = TRUE) |>
    sf::st_transform(4326)
  message(glue::glue("{nrow(catchments_4326)} catchments loaded"))

  message("Extracting phosphorus surplus per catchment...")
  result <- exactextractr::exact_extract(ensemble_rast, catchments_4326, fun = "mean", append_cols = "WSO1_ID") |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("mean.Y"),
      names_to = "year",
      values_to = "p_surplus_kg_ha"
    ) |>
    dplyr::mutate(year = as.integer(gsub("mean\\.Y", "", year)))

  # Coverage summary
  n_valid <- sum(!is.na(result$p_surplus_kg_ha))
  message(glue::glue("Output: {nrow(result)} rows, {length(unique(result$WSO1_ID))} catchments x {length(unique(result$year))} years"))
  message(glue::glue("Non-NA values: {n_valid} ({round(100 * n_valid / nrow(result), 1)}%)"))

  # Write output
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  arrow::write_parquet(result, output_path)
  message(glue::glue("Written to {output_path}"))
  invisible(result)
}
