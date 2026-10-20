# Batched climate catchment extraction for machines with limited RAM (e.g. 16 GB)
# Same outputs as 0202_extract_climate.R (climate_{var}_{stat}.parquet with WSO1_ID, variable, stat, year, month, value),
# but catchments are read and extracted in batches by row ID, each batch is written to a parquet part,
# and the parts are merged into one file at the end
# Finished parts are kept until the merge succeeds, so rerunning after a crash resumes where it stopped

# Read one batch of catchments by row ID and reproject to WGS84 to match E-OBS
read_catchment_batch <- function(catchment_path, first_row, last_row, layer = "EU_primary_catchments") {
  query <- glue::glue('SELECT * FROM "{layer}" WHERE rowid >= {first_row} AND rowid <= {last_row}')
  sf::st_read(catchment_path, query = query, quiet = TRUE) |>
    sf::st_transform(4326)
}

# Area-weighted mean of every monthly layer for one batch of catchments, in long format
extract_monthly_batch <- function(r, var_code, stat_label, catch) {
  ex <- exactextractr::exact_extract(r, catch, fun = "mean", progress = FALSE)

  catch |>
    sf::st_drop_geometry() |>
    dplyr::select(WSO1_ID) |>
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

# Merge the parquet parts into one file, appending one part at a time so only one part is in memory
# Can also be run on its own for parts left behind by an earlier run, e.g.
# merge_parquet_parts("data/drivers/climate/outputs/_parts_climate_hu_max", "data/drivers/climate/outputs/climate_hu_max.parquet")
merge_parquet_parts <- function(parts_dir, out_path, n_expected = NULL) {
  parts <- sort(list.files(parts_dir, pattern = "^part_.*\\.parquet$", full.names = TRUE))
  tmp_path <- paste0(out_path, ".tmp")

  # Open the output file with the column layout of the first part
  first <- arrow::read_parquet(parts[1], as_data_frame = FALSE)
  sink <- arrow::FileOutputStream$create(tmp_path)
  writer <- arrow::ParquetFileWriter$create(
    first$schema, sink,
    properties = arrow::ParquetWriterProperties$create(names(first$schema))
  )

  # Append each part and count the rows
  n_rows <- 0
  for (p in parts) {
    part <- arrow::read_parquet(p, as_data_frame = FALSE)
    writer$WriteTable(part, chunk_size = 1e6)
    n_rows <- n_rows + part$num_rows
  }
  writer$Close()
  sink$close()

  # Keep the parts and stop if any rows are missing
  if (!is.null(n_expected) && n_rows != n_expected) {
    stop("Merged ", n_rows, " rows, expected ", n_expected, "; parts kept in ", parts_dir)
  }

  file.rename(tmp_path, out_path)
  message(glue::glue("  Merged {length(parts)} parts, {n_rows} rows"))
  invisible(out_path)
}

# Extract the chosen variables and stats in batches of catchments
# vars: any of "tg", "tn", "tx", "rr", "hu"; stats: any of "mean", "max", "min" ("mean" is written as "sum" for rr)
# Existing outputs are skipped unless overwrite = TRUE
extract_climate_batched <- function(catchment_path, monthly_dir, out_dir = monthly_dir,
                                    vars = c("tg", "tn", "tx", "rr", "hu"),
                                    stats = c("mean", "max", "min"),
                                    batch_size = 100000,
                                    overwrite = FALSE,
                                    layer = "EU_primary_catchments") {
  # Stop early on typos so nothing runs for nothing
  stopifnot(
    "vars must be in tg, tn, tx, rr, hu" = all(vars %in% c("tg", "tn", "tx", "rr", "hu")),
    "stats must be in mean, max, min" = all(stats %in% c("mean", "max", "min"))
  )
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Row ID range and count of the catchment layer, read without loading any geometry
  con <- DBI::dbConnect(RSQLite::SQLite(), catchment_path, flags = RSQLite::SQLITE_RO)
  row_range <- DBI::dbGetQuery(con, glue::glue('SELECT MIN(rowid) AS lo, MAX(rowid) AS hi, COUNT(*) AS n FROM "{layer}"'))
  DBI::dbDisconnect(con)
  batch_starts <- seq(row_range$lo, row_range$hi, by = batch_size)
  message(glue::glue("{row_range$n} catchments in {length(batch_starts)} batches of up to {batch_size}"))

  # Loop over variables and stats
  for (var_code in intersect(c("tg", "tn", "tx", "rr", "hu"), vars)) {
    for (stat_file in intersect(c("mean", "max", "min"), stats)) {
      nc_path <- file.path(monthly_dir, glue::glue("{var_code}_{stat_file}_monthly.nc"))
      if (!file.exists(nc_path)) {
        warning(glue::glue("Not found: {nc_path} -- skipping"))
        next
      }

      out_label <- if (stat_file == "mean" && var_code == "rr") "sum" else stat_file
      out_path <- file.path(out_dir, glue::glue("climate_{var_code}_{out_label}.parquet"))
      if (file.exists(out_path) && !overwrite) {
        message(glue::glue("  Exists: {basename(out_path)}"))
        next
      }

      # Parts for this output; overwrite starts from scratch, otherwise finished parts are reused
      parts_dir <- file.path(out_dir, glue::glue("_parts_climate_{var_code}_{out_label}"))
      if (overwrite) unlink(parts_dir, recursive = TRUE)
      dir.create(parts_dir, showWarnings = FALSE)

      # Monthly raster, layers named by year and month
      r <- terra::rast(nc_path)
      names(r) <- format(terra::time(r), "%Y_%m")
      message(glue::glue("Extracting {var_code} {out_label}: {terra::nlyr(r)} months"))

      # Extract each batch and write it as a part, writing to a temporary name first so a crash never leaves a half-written part
      for (i in seq_along(batch_starts)) {
        part_path <- file.path(parts_dir, sprintf("part_%04d.parquet", i))
        if (file.exists(part_path)) next

        t0 <- Sys.time()
        first_row <- batch_starts[i]
        last_row <- min(first_row + batch_size - 1, row_range$hi)
        catch <- read_catchment_batch(catchment_path, first_row, last_row, layer)
        if (nrow(catch) > 0) {
          result <- extract_monthly_batch(r, var_code, out_label, catch)
          arrow::write_parquet(result, paste0(part_path, ".tmp"))
          file.rename(paste0(part_path, ".tmp"), part_path)
          rm(result)
        }
        elapsed <- round(difftime(Sys.time(), t0, units = "mins"), 1)
        message(glue::glue("  Batch {i}/{length(batch_starts)}: {nrow(catch)} catchments ({elapsed} min)"))
        rm(catch)
        gc()
      }

      # Merge the parts into the final file and remove them
      message(glue::glue("  Merging parts into {basename(out_path)}..."))
      merge_parquet_parts(parts_dir, out_path, n_expected = row_range$n * terra::nlyr(r))
      unlink(parts_dir, recursive = TRUE)
      message(glue::glue("  Written: {basename(out_path)}"))
    }
  }

  invisible(out_dir)
}
