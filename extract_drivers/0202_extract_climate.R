# Climate catchment extraction
# Extracts monthly climate statistics from the preprocessed E-OBS monthly NetCDFs per EU primary catchment (WSO1_ID)
#
# Source: E-OBS ensemble mean, 0.1deg regular grid, WGS84, v33.0e (1995-2010), v27.0e (2011-2022)
# Variables: tg (mean temp, C), tn (min temp, C), tx (max temp, C), rr (rainfall, mm), hu (relative humidity, %)
# Monthly statistics per variable: mean (or sum for rr), max and min of the daily values in the month
#
# Extraction: area-weighted mean of each monthly raster per catchment
# NODATA: -9999 in source, read as NA by terra. na.rm = TRUE during daily-to-monthly aggregation,
# so months with partial daily coverage still produce a value
#
# Output: one parquet per variable and stat, climate_{var}_{stat}.parquet in out_dir
#   Columns: WSO1_ID (int), variable (chr), stat (chr), year (int), month (int), value (dbl)

# Extract one monthly raster file per catchment and reshape to long format
extract_monthly_file <- function(nc_path, var_code, stat_label, catch, wso_ids) {
  r <- terra::rast(nc_path)

  # Parse year-month from the time dimension
  names(r) <- format(terra::time(r), "%Y_%m")

  message(glue::glue("  Extracting {var_code} {stat_label}: {terra::nlyr(r)} months..."))
  ex <- exactextractr::exact_extract(r, catch, fun = "mean", progress = TRUE)

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

# Extract the chosen variables and stats, writing each to its own parquet to keep memory down
# vars: any of "tg", "tn", "tx", "rr", "hu"; stats: any of "mean", "max", "min" ("mean" is written as "sum" for rr)
# Existing outputs are skipped unless overwrite = TRUE
extract_climate <- function(catchment_path, monthly_dir, out_dir = monthly_dir,
                            vars = c("tg", "tn", "tx", "rr", "hu"),
                            stats = c("mean", "max", "min"),
                            overwrite = FALSE) {
  # Variables and their central stat label, sum for rainfall and mean for everything else
  variables <- tibble::tribble(
    ~var_code, ~central_label,
    "tg",      "mean",
    "tn",      "mean",
    "tx",      "mean",
    "rr",      "sum",
    "hu",      "mean"
  ) |>
    dplyr::filter(var_code %in% vars)
  stat_files <- intersect(c("mean", "max", "min"), stats)

  # Stop early on typos so nothing runs for nothing
  stopifnot(
    "vars must be in tg, tn, tx, rr, hu" = all(vars %in% c("tg", "tn", "tx", "rr", "hu")),
    "stats must be in mean, max, min" = all(stats %in% c("mean", "max", "min"))
  )

  # Load catchments and reproject to WGS84
  message("Loading catchments...")
  catchments_wgs84 <- sf::st_read(catchment_path, layer = "EU_primary_catchments", quiet = TRUE) |>
    sf::st_transform(4326)
  message(glue::glue("  {nrow(catchments_wgs84)} catchments reprojected to EPSG:4326"))

  wso_ids <- catchments_wgs84 |>
    sf::st_drop_geometry() |>
    dplyr::select(WSO1_ID)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Loop over variables and stats, skipping outputs that already exist
  for (v_i in seq_len(nrow(variables))) {
    v <- variables[v_i, ]

    for (stat_file in stat_files) {
      nc_path <- file.path(monthly_dir, glue::glue("{v$var_code}_{stat_file}_monthly.nc"))
      if (!file.exists(nc_path)) {
        warning(glue::glue("Not found: {nc_path} -- skipping"))
        next
      }

      out_label <- if (stat_file == "mean") v$central_label else stat_file
      chunk_path <- file.path(out_dir, glue::glue("climate_{v$var_code}_{out_label}.parquet"))
      if (file.exists(chunk_path) && !overwrite) {
        message(glue::glue("  Exists: {basename(chunk_path)}"))
        next
      }

      result <- extract_monthly_file(nc_path, v$var_code, out_label, catchments_wgs84, wso_ids)
      message(glue::glue("  -> {nrow(result)} rows, writing parquet..."))
      arrow::write_parquet(result, chunk_path)

      rm(result)
      gc()
    }
  }

  invisible(out_dir)
}
