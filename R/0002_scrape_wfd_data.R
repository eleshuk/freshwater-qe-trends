# Functions to download WFD tables from Discodata and convert them to parquet

# Download one zip file, retrying on failure and skipping files that already exist
download_zip <- function(url, dest_dir, max_attempts = 3) {
  fs::dir_create(dest_dir)
  
  table <- basename(url)
  dest <- fs::path(dest_dir, paste0(table, ".zip"))
  
  # Return a one-row status summary for this download
  status_row <- function(status) {
    size_mb <- if (fs::file_exists(dest)) round(file.info(dest)$size / 1e6, 2) else NA_real_
    data.frame(table = table, status = status, size_MB = size_mb, url = url)
  }
  
  # Skip if already downloaded
  if (fs::file_exists(dest) && file.info(dest)$size > 1000) {
    message("Already exists: ", table)
    return(status_row("skipped"))
  }
  
  # Try the download up to max_attempts times
  for (attempt in seq_len(max_attempts)) {
    message("Downloading (attempt ", attempt, "): ", table)
    
    try_result <- try(
      utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE),
      silent = TRUE
    )
    
    # Stop early on a hard 404
    if (inherits(try_result, "try-error") && grepl("404", try_result, ignore.case = TRUE)) {
      if (fs::file_exists(dest)) fs::file_delete(dest)
      message("FAILED (404): ", table)
      return(status_row("missing_404"))
    }
    
    if (!inherits(try_result, "try-error") && fs::file_exists(dest) && file.info(dest)$size > 1000) {
      message("Success: ", table, " (", round(file.info(dest)$size / 1e6, 2), " MB)")
      return(status_row("downloaded"))
    }
    
    # Remove a bad or partial file so the next attempt is clean
    if (fs::file_exists(dest)) fs::file_delete(dest)
    Sys.sleep(stats::runif(1, 2, 6))
  }
  
  message("FAILED: ", table)
  status_row("failed")
}

# Download every table listed in the links spreadsheet and return a status table
download_wfd_zips <- function(links_file, dest_dir, max_attempts = 3) {
  links <- readxl::read_excel(links_file)$url
  
  # Use libcurl and a long timeout for large files, restoring options afterwards
  old_options <- options(download.file.method = "libcurl", timeout = 7200)
  on.exit(options(old_options))
  
  # Pause between downloads to not overload the server
  links |>
    purrr::map(\(u) {
      res <- download_zip(u, dest_dir = dest_dir, max_attempts = max_attempts)
      Sys.sleep(stats::runif(1, 0.5, 1.5))
      res
    }) |>
    purrr::list_rbind()
}

# Delete downloaded zips smaller than min_size bytes
remove_bad_zips <- function(dest_dir, min_size = 1e6) {
  files <- fs::dir_ls(dest_dir, regexp = "\\.zip$")
  bad <- files[file.info(files)$size < min_size]
  
  if (length(bad) > 0) {
    message("Removing ", length(bad), " small file(s)")
    fs::file_delete(bad)
  }
  
  invisible(bad)
}

# Turn a zip file path into a snake_case table name
table_name_from_zip <- function(zipfile) {
  x <- fs::path_ext_remove(fs::path_file(zipfile))
  
  # Split camel case and collapse non-alphanumeric characters into underscores
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", x)
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  
  tolower(x)
}

# Read one csv inside a zip and write it to a parquet dataset
convert_one_csv <- function(zipfile, csv_path_in_zip, parquet_root) {
  out_dir <- fs::path(parquet_root, table_name_from_zip(zipfile))
  fs::dir_create(out_dir)
  
  # Store everything as character, cast types explicitly in analysis
  df <- vroom::vroom(
    unz(zipfile, csv_path_in_zip),
    delim = ";",
    col_types = vroom::cols(.default = vroom::col_character()),
    progress = FALSE
  )
  
  # Name parquet files after the csv so several csvs in one zip don't overwrite each other
  csv_stem <- fs::path_ext_remove(fs::path_file(csv_path_in_zip))
  
  arrow::write_dataset(
    df,
    path = out_dir,
    format = "parquet",
    basename_template = paste0(csv_stem, "-{i}.parquet"),
    existing_data_behavior = "overwrite"
  )
  
  invisible(out_dir)
}

# Convert every csv in every zip in zip_dir and save parquet output to parquet_root
convert_wfd_to_parquet <- function(zip_dir, parquet_root) {
  fs::dir_create(parquet_root)
  
  zip_files <- fs::dir_ls(zip_dir, regexp = "\\.zip$", recurse = FALSE)
  
  if (length(zip_files) == 0) {
    message("No zip files found in ", zip_dir)
    return(invisible(parquet_root))
  }
  
  # Loop over zips, then over the csvs inside each zip
  for (z in zip_files) {
    message("ZIP: ", z)
    
    inside <- utils::unzip(z, list = TRUE)$Name
    csvs <- inside[stringr::str_detect(inside, stringr::regex("\\.csv$", ignore_case = TRUE))]
    
    if (length(csvs) == 0) {
      message("  (no csv files found)")
      next
    }
    
    for (csv in csvs) {
      message("  CSV: ", csv)
      tryCatch(
        convert_one_csv(z, csv, parquet_root),
        error = function(e) message("    !! failed: ", conditionMessage(e))
      )
    }
  }
  
  invisible(parquet_root)
}