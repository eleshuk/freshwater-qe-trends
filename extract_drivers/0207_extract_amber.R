# AMBER Barrier Atlas: per-catchment barrier counts
# Point-in-polygon spatial join of AMBER barriers to EU primary catchments (CCM2)
# Output: amber_catchment.parquet with WSO1_ID, Type, n_barriers, n_barriers_total

# Count AMBER barriers per catchment, in total and by barrier type
extract_amber <- function(amber_path, catchment_path, output_path) {
  # Read AMBER barriers (semicolon-delimited, WGS84) and convert coordinates to numeric
  amber_raw <- utils::read.csv2(amber_path, stringsAsFactors = FALSE) |>
    dplyr::mutate(
      Longitude_WGS84 = as.numeric(Longitude_WGS84),
      Latitude_WGS84 = as.numeric(Latitude_WGS84)
    )
  message("AMBER records read: ", nrow(amber_raw))

  # Drop records with missing coordinates
  amber_valid <- amber_raw |>
    dplyr::filter(!is.na(Longitude_WGS84), !is.na(Latitude_WGS84))
  message("Records dropped (missing coordinates): ", nrow(amber_raw) - nrow(amber_valid))

  # Build sf point layer and reproject to EPSG:3035
  amber_sf <- amber_valid |>
    sf::st_as_sf(coords = c("Longitude_WGS84", "Latitude_WGS84"), crs = 4326) |>
    sf::st_transform(3035)

  # Read catchments
  catchments <- sf::st_read(catchment_path, layer = "EU_primary_catchments", quiet = TRUE)
  message("Catchments loaded: ", nrow(catchments))

  # Assign each barrier its enclosing catchment's WSO1_ID
  amber_catch <- sf::st_join(amber_sf, catchments[, "WSO1_ID"], join = sf::st_within)
  message("Barriers outside catchment coverage: ", sum(is.na(amber_catch$WSO1_ID)))

  amber_catch <- amber_catch |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(WSO1_ID))

  # Count by barrier type per catchment and join the total count
  total_count <- amber_catch |>
    dplyr::summarise(n_barriers_total = dplyr::n(), .by = WSO1_ID)

  amber_summary <- amber_catch |>
    dplyr::summarise(n_barriers = dplyr::n(), .by = c(WSO1_ID, Type)) |>
    dplyr::left_join(total_count, by = "WSO1_ID")

  message("Output rows: ", nrow(amber_summary))
  message("Catchments with at least one barrier: ", dplyr::n_distinct(amber_summary$WSO1_ID))

  # Write output
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  arrow::write_parquet(amber_summary, output_path)
  message("Written to: ", output_path)
  invisible(amber_summary)
}
