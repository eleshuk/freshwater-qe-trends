# Land cover extraction per catchment
# Extracts CORINE Land Cover area (m2) per class for each EU_primary_catchments polygon, across CLC 2006, 2012 and 2018
# Output: clc_catchment_area_by_class.parquet with WSO1_ID, clc_year, clc_code, clc_label, area_m2

# CLC legend mapping raster codes to classes and labels
clc_legend <- tibble::tribble(
  ~clc_code, ~clc_class, ~clc_label,
  1L,  "111", "Continuous urban fabric",
  2L,  "112", "Discontinuous urban fabric",
  3L,  "121", "Industrial or commercial units",
  4L,  "122", "Road and rail networks and associated land",
  5L,  "123", "Port areas",
  6L,  "124", "Airports",
  7L,  "131", "Mineral extraction sites",
  8L,  "132", "Dump sites",
  9L,  "133", "Construction sites",
  10L, "141", "Green urban areas",
  11L, "142", "Sport and leisure facilities",
  12L, "211", "Non-irrigated arable land",
  13L, "212", "Permanently irrigated land",
  14L, "213", "Rice fields",
  15L, "221", "Vineyards",
  16L, "222", "Fruit trees and berry plantations",
  17L, "223", "Olive groves",
  18L, "231", "Pastures",
  19L, "241", "Annual crops associated with permanent crops",
  20L, "242", "Complex cultivation patterns",
  21L, "243", "Land principally occupied by agriculture with significant areas of natural vegetation",
  22L, "244", "Agro-forestry areas",
  23L, "311", "Broad-leaved forest",
  24L, "312", "Coniferous forest",
  25L, "313", "Mixed forest",
  26L, "321", "Natural grasslands",
  27L, "322", "Moors and heathland",
  28L, "323", "Sclerophyllous vegetation",
  29L, "324", "Transitional woodland-shrub",
  30L, "331", "Beaches dunes sands",
  31L, "332", "Bare rocks",
  32L, "333", "Sparsely vegetated areas",
  33L, "334", "Burnt areas",
  34L, "335", "Glaciers and perpetual snow",
  35L, "411", "Inland marshes",
  36L, "412", "Peat bogs",
  37L, "421", "Salt marshes",
  38L, "422", "Salines",
  39L, "423", "Intertidal flats",
  40L, "511", "Water courses",
  41L, "512", "Water bodies",
  42L, "521", "Coastal lagoons",
  43L, "522", "Estuaries",
  44L, "523", "Sea and ocean"
)

# Assign catchments to batches by centroid Y quantiles so each cropped raster stays under the cell limit
assign_clc_batches <- function(catchments, probe_raster_path, target_cells_per_batch = 1e9) {
  centroids_y <- sf::st_coordinates(sf::st_centroid(catchments, of_largest_polygon = TRUE))[, 2]

  # Estimate total cells from the first CLC raster cropped to the catchments
  probe_rast <- terra::rast(probe_raster_path)
  catch_ext <- terra::ext(terra::vect(sf::st_bbox(catchments) |> sf::st_as_sfc()))
  catch_ext <- terra::extend(catch_ext, 1000)
  probe_crop <- terra::crop(probe_rast, catch_ext)
  total_cells <- as.numeric(ncol(probe_crop)) * as.numeric(nrow(probe_crop))
  n_batches <- max(2L, ceiling(total_cells / target_cells_per_batch))
  message(sprintf("  Total raster cells: %.2g -> using %d batches", total_cells, n_batches))

  # Assign every catchment to a batch via quantile breaks
  breaks <- stats::quantile(centroids_y, probs = seq(0, 1, length.out = n_batches + 1), na.rm = TRUE)
  breaks[1] <- breaks[1] - 1
  breaks[length(breaks)] <- breaks[length(breaks)] + 1
  batch_ids <- findInterval(centroids_y, breaks, rightmost.closed = TRUE)

  # Catchments with NA centroids (empty/null geometry) go into a rescue batch
  na_mask <- is.na(batch_ids)
  if (any(na_mask)) {
    message(sprintf("  %d catchments with NA centroids assigned to rescue batch", sum(na_mask)))
    batch_ids[na_mask] <- n_batches + 1L
    n_batches <- n_batches + 1L
  }

  batch_counts <- table(batch_ids)
  message(sprintf("  Batch sizes: %s", paste(sprintf("%s=%d", names(batch_counts), batch_counts), collapse = ", ")))
  stopifnot(sum(is.na(batch_ids)) == 0)
  stopifnot(length(batch_ids) == nrow(catchments))

  list(batch_ids = batch_ids, n_batches = n_batches)
}

# Extract land cover area by class for one CLC year
extract_clc_area <- function(catchments, batch_ids, n_batches,
                             raster_path, year_label) {
  message(sprintf("\nProcessing CLC %s...", year_label))
  clc_rast <- terra::rast(raster_path)
  
  # CRS check
  catch_epsg <- sf::st_crs(catchments)$epsg
  rast_epsg <- as.integer(terra::crs(clc_rast, describe = TRUE)$code)
  if (is.na(catch_epsg) || is.na(rast_epsg) || catch_epsg != rast_epsg) {
    stop(sprintf("CRS mismatch: catchments EPSG=%s, raster EPSG=%s", catch_epsg, rast_epsg))
  }
  
  # Crop to full catchment extent
  catch_ext <- terra::ext(terra::vect(sf::st_bbox(catchments) |> sf::st_as_sfc()))
  catch_ext <- terra::extend(catch_ext, 1000)
  clc_crop <- terra::crop(clc_rast, catch_ext)
  
  # Reclassify background (0), CLC NODATA value (48), and metadata NODATA (128) to NA
  rcl <- matrix(c(0, NA, 48, NA, 128, NA), ncol = 2, byrow = TRUE)
  clc_crop <- terra::classify(clc_crop, rcl)
  message(sprintf("  Raster cropped: %d x %d pixels", ncol(clc_crop), nrow(clc_crop)))
  
  pixel_area <- 10000
  
  # Extract per batch
  batch_results <- vector("list", n_batches)
  for (b in seq_len(n_batches)) {
    idx <- which(batch_ids == b)
    if (length(idx) == 0) next
    message(sprintf("  Batch %d/%d: %d catchments", b, n_batches, length(idx)))
    
    batch <- catchments[idx, ]
    batch_ext <- terra::ext(terra::vect(sf::st_bbox(batch) |> sf::st_as_sfc()))
    batch_ext <- terra::extend(batch_ext, 1000)
    batch_crop <- terra::crop(clc_crop, batch_ext)
    message(sprintf("    Cropped: %d x %d pixels", ncol(batch_crop), nrow(batch_crop)))
    
    batch_results[[b]] <- exactextractr::exact_extract(
      batch_crop, batch,
      fun = c("frac", "count"),
      append_cols = "WSO1_ID",
      max_cells_in_memory = 2000000000L,
      progress = TRUE
    )
    rm(batch_crop)
  }
  
  result <- dplyr::bind_rows(batch_results)
  rm(batch_results, clc_crop)
  gc()
  
  # Pivot frac columns to long format and convert to area
  frac_cols <- grep("^frac_", names(result), value = TRUE)
  long <- result |>
    tidyr::pivot_longer(cols = dplyr::all_of(frac_cols),
                        names_to = "clc_code",
                        values_to = "frac") |>
    dplyr::filter(!is.na(frac) & frac > 0) |>
    dplyr::mutate(
      clc_code = as.integer(sub("frac_", "", clc_code)),
      area_m2 = frac * count * pixel_area
    ) |>
    dplyr::select(WSO1_ID, clc_code, area_m2)
  
  long$clc_year <- year_label
  
  # Post-extraction coverage check
  extracted_ids <- unique(long$WSO1_ID)
  missing_ids <- setdiff(catchments$WSO1_ID, extracted_ids)
  message(sprintf("  CLC %s: %d catchments with data, %d with no valid CLC pixels",
                  year_label, length(extracted_ids), length(missing_ids)))
  
  message(sprintf("  Done: %d rows", nrow(long)))
  long
}

# Extract land cover for every CLC year, run sanity checks and write the output
extract_land_cover <- function(clc_base, catchment_path, output_parquet,
                               catchment_layer = "EU_primary_catchments") {
  clc_paths <- c(
    "2006" = file.path(clc_base, "u2012_clc2006_v2020_20u1_raster100m/DATA/U2012_CLC2006_V2020_20u1.tif"),
    "2012" = file.path(clc_base, "u2018_clc2012_v2020_20u1_raster100m/DATA/U2018_CLC2012_V2020_20u1.tif"),
    "2018" = file.path(clc_base, "u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")
  )

  # Read catchments
  message("Reading catchment polygons...")
  catchments <- sf::st_read(catchment_path, layer = catchment_layer, quiet = TRUE)
  n_input <- nrow(catchments)
  message(sprintf("  %d catchments loaded, CRS: %s", n_input, sf::st_crs(catchments)$input))

  # Batch assignments are computed once and reused across all CLC years
  message("Computing batch assignments...")
  batches <- assign_clc_batches(catchments, clc_paths[[1]])

  # Run for all three years and attach labels
  all_results <- names(clc_paths) |>
    purrr::map(\(yr) {
      res <- extract_clc_area(catchments, batches$batch_ids, batches$n_batches, clc_paths[[yr]], yr)
      gc()
      res
    }) |>
    purrr::list_rbind() |>
    dplyr::left_join(clc_legend, by = "clc_code") |>
    dplyr::select(WSO1_ID, clc_year, clc_code, clc_label, area_m2)

  # Sanity checks
  message("\n--- Summary ---")
  message(sprintf("Total rows: %d", nrow(all_results)))
  n_extracted <- dplyr::n_distinct(all_results$WSO1_ID)
  message(sprintf("Unique catchments with CLC data: %d / %d (%.1f%%)",
                  n_extracted, n_input, 100 * n_extracted / n_input))
  message(sprintf("Unique CLC classes found: %d", dplyr::n_distinct(all_results$clc_code)))
  message(sprintf("Years: %s", paste(unique(all_results$clc_year), collapse = ", ")))

  # Flag unmatched legend codes
  unmatched <- all_results |>
    dplyr::filter(is.na(clc_label)) |>
    dplyr::distinct(clc_code)
  if (nrow(unmatched) > 0) {
    warning(sprintf("Unmatched CLC codes: %s", paste(unmatched$clc_code, collapse = ", ")))
  } else {
    message("All CLC codes matched to legend labels.")
  }

  # Report catchments missing across all years
  globally_missing <- setdiff(catchments$WSO1_ID, unique(all_results$WSO1_ID))
  message(sprintf("Catchments with no CLC data in any year: %d (%.1f%%)",
                  length(globally_missing), 100 * length(globally_missing) / n_input))

  # Area ratio check of total CLC area against catchment area
  check_year <- "2018"
  comparison <- all_results |>
    dplyr::filter(clc_year == check_year) |>
    dplyr::summarise(clc_total_m2 = sum(area_m2), .by = WSO1_ID) |>
    dplyr::inner_join(
      catchments |>
        sf::st_drop_geometry() |>
        dplyr::select(WSO1_ID, PC_area),
      by = "WSO1_ID"
    ) |>
    dplyr::mutate(ratio = clc_total_m2 / PC_area)
  message(sprintf(
    "Area ratio (CLC %s total / PC_area): median=%.3f, min=%.3f, max=%.3f",
    check_year, stats::median(comparison$ratio), min(comparison$ratio), max(comparison$ratio)
  ))

  # Write output
  dir.create(dirname(output_parquet), showWarnings = FALSE, recursive = TRUE)
  message(sprintf("\nWriting results to %s", output_parquet))
  arrow::write_parquet(all_results, output_parquet)
  invisible(all_results)
}
