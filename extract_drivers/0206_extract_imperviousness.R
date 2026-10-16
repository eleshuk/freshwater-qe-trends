# Imperviousness Density extraction per EU primary catchment
# Extracts mean imperviousness (%) and total impervious area (m2) per catchment for all five IMD reference years (2006-2018)
# Uses spatial batching (north/south) to avoid the 32-bit integer limit on max_cells_in_memory in exactextractr
#
# Input: IMD GeoTIFFs inside zip archives in imd_dir
# Output: imperviousness_catchment.parquet with WSO1_ID, year, mean_imperviousness_pct, impervious_area_m2

# Extract imperviousness per catchment for every IMD year
extract_imperviousness <- function(imd_dir, catchment_path, output_path, pixel_area_m2 = 10000) {
  imd_files <- tibble::tibble(
    year = c(2006L, 2009L, 2012L, 2015L, 2018L),
    zip = file.path(imd_dir, c(
      "IMD_2006_100m_eu_03035_d03_Full.zip",
      "IMD_2009_100m_eu_03035_d03_Full.zip",
      "IMD_2012_100m_eu_03035_d03_Full.zip",
      "IMD_2015_100m_eu_03035_d03_Full.zip",
      "IMD_2018_100m_eu_03035_v020.zip"
    )),
    tif_path = c(
      "IMD_2006_100m_eu_03035_d03_Full/IMD_2006_100m_eu_03035_d03_full.tif",
      "IMD_2009_100m_eu_03035_d03_Full/IMD_2009_100m_eu_03035_d03_full.tif",
      "IMD_2012_100m_eu_03035_d03_Full/IMD_2012_100m_eu_03035_d03_full.tif",
      "IMD_2015_100m_eu_03035_d03_Full/IMD_2015_100m_eu_03035_d03_full.tif",
      "IMD_2018_100m_eu_03035_v020/DATA/IMD_2018_100m_eu_03035_V2_0.tif"
    )
  )

  # Extract TIFs from zip archives to a temporary directory
  tmp_dir <- file.path(tempdir(), "imd_extract")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  message("Extracting TIF files from zip archives...")
  tif_paths <- character(nrow(imd_files))
  for (i in seq_len(nrow(imd_files))) {
    dest <- file.path(tmp_dir, basename(imd_files$tif_path[i]))
    if (!file.exists(dest)) {
      utils::unzip(imd_files$zip[i], files = imd_files$tif_path[i], exdir = tmp_dir, junkpaths = TRUE)
    }
    tif_paths[i] <- dest
    message(sprintf("  %d: %s", imd_files$year[i], basename(dest)))
  }

  # Load catchment polygons
  message(sprintf("Loading catchments from %s...", catchment_path))
  catchments <- sf::st_read(catchment_path, layer = "EU_primary_catchments", quiet = TRUE)
  message(sprintf("  %s catchments loaded", format(nrow(catchments), big.mark = ",")))
  stopifnot("Catchment CRS must be EPSG:3035" = sf::st_crs(catchments)$epsg == 3035)

  # Split catchments into north/south batches using the median centroid Y-coordinate
  centroids <- sf::st_coordinates(sf::st_centroid(catchments))
  median_y <- stats::median(centroids[, "Y"])
  batches <- list(
    south = catchments[which(centroids[, "Y"] <= median_y), ],
    north = catchments[which(centroids[, "Y"] > median_y), ]
  )
  message(sprintf("Spatial batches: south = %s catchments, north = %s catchments",
                  format(nrow(batches$south), big.mark = ","),
                  format(nrow(batches$north), big.mark = ",")))

  # Reclassification matrix: 254 (unclassifiable) and 255 (outside coverage) -> NA
  rcl_matrix <- matrix(c(254, NA, 255, NA), ncol = 2, byrow = TRUE)

  # Process each year individually to avoid stacking ~15 billion cells
  all_results <- list()
  for (i in seq_len(nrow(imd_files))) {
    yr <- imd_files$year[i]
    message(sprintf("\n=== Processing IMD %d ===", yr))

    r <- terra::rast(tif_paths[i])
    r <- terra::classify(r, rcl_matrix, right = NA)

    # Harmonise CRS (older DATUM-based vs newer ENSEMBLE-based WKT)
    terra::crs(r) <- terra::crs(terra::vect(catchments))

    # Extract mean and sum for each batch
    batch_results <- list()
    for (batch_name in names(batches)) {
      batch <- batches[[batch_name]]

      message(sprintf("  Batch %s: cropping raster...", batch_name))
      batch_crop <- terra::crop(r, terra::vect(batch), snap = "out")
      message(sprintf("  Batch %s: cropped to %d x %d pixels (%s cells)",
                      batch_name, ncol(batch_crop), nrow(batch_crop),
                      format(as.numeric(ncol(batch_crop)) * nrow(batch_crop), big.mark = ",")))

      message(sprintf("  Batch %s: extracting mean and sum...", batch_name))
      ex <- exactextractr::exact_extract(
        batch_crop, batch,
        fun = c("mean", "sum"),
        append_cols = "WSO1_ID",
        max_cells_in_memory = 2000000000L,
        progress = TRUE
      )

      # Rename columns from mean / mean.lyr.1 and sum / sum.lyr.1 to something predictable
      names(ex)[names(ex) == "mean"] <- "mean_imperviousness_pct"
      names(ex)[names(ex) == "sum"] <- "sum_raw"
      names(ex) <- gsub("^mean\\..*", "mean_imperviousness_pct", names(ex))
      names(ex) <- gsub("^sum\\..*", "sum_raw", names(ex))

      ex$year <- yr
      ex$impervious_area_m2 <- ex$sum_raw * (pixel_area_m2 / 100)
      batch_results[[batch_name]] <- ex |>
        dplyr::select(WSO1_ID, year, mean_imperviousness_pct, impervious_area_m2)

      rm(batch_crop)
      gc()
    }

    all_results[[as.character(yr)]] <- dplyr::bind_rows(batch_results)
    rm(r)
    gc()
    message(sprintf("  IMD %d complete: %s rows", yr, format(nrow(all_results[[as.character(yr)]]), big.mark = ",")))
  }

  # Combine all years
  result <- dplyr::bind_rows(all_results) |>
    dplyr::arrange(WSO1_ID, year)
  message(sprintf("\nResult: %s rows, %s catchments, %d years",
                  format(nrow(result), big.mark = ","),
                  format(dplyr::n_distinct(result$WSO1_ID), big.mark = ","),
                  dplyr::n_distinct(result$year)))

  # Verification summary
  result |>
    dplyr::summarise(
      mean_pct = mean(mean_imperviousness_pct, na.rm = TRUE),
      max_pct = max(mean_imperviousness_pct, na.rm = TRUE),
      total_impervious_km2 = sum(impervious_area_m2, na.rm = TRUE) / 1e6,
      n_na = sum(is.na(mean_imperviousness_pct)),
      .by = year
    ) |>
    print()

  # Write output
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  arrow::write_parquet(result, output_path)
  message(sprintf("Written to %s", output_path))
  invisible(result)
}
