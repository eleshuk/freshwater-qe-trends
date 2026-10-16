# Climate preprocessing for E-OBS climate data
# Filters daily NetCDFs to 2004-2022, aggregates to monthly mean/max/min (sum for rainfall),
# and merges the two source periods into one continuous file per variable and statistic
# Two methods: "cdo" needs CDO installed (brew install cdo or apt install cdo, ~4 s per aggregation),
# "windows" is pure R with terra and has no external dependencies (~1-1.5 min per aggregation)

# Source periods of the E-OBS files
climate_periods <- list(
  list(folder = "1995-2010_climate", suffix = "1995-2010_v33.0e", tag = "1995-2010"),
  list(folder = "2011-2022_climate", suffix = "2011-2022_v27.0e", tag = "2011-2022")
)

# Variables to aggregate, rainfall uses a sum for central tendency
climate_vars <- c("tg", "tn", "tx", "rr", "hu")

# Run the chosen preprocessing method
preprocess_climate <- function(climate_base, out_dir = file.path(climate_base, "outputs"),
                               method = c("cdo", "windows"), years = 2004:2022) {
  method <- match.arg(method)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  if (method == "cdo") {
    preprocess_climate_cdo(climate_base, out_dir, years)
  } else {
    preprocess_climate_windows(climate_base, out_dir, years)
  }

  message(glue::glue("\nDone. Monthly files in: {out_dir}"))
  invisible(out_dir)
}

# Monthly aggregation and period merge with CDO
preprocess_climate_cdo <- function(climate_base, out_dir, years) {
  # Check CDO is available
  if (system("cdo --version", ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
    stop("CDO not found. Install with brew install cdo (macOS) or apt install cdo (Linux), or use method = \"windows\"")
  }

  year_range <- glue::glue("{min(years)}/{max(years)}")
  var_stats <- climate_vars |>
    purrr::set_names() |>
    purrr::map(\(v) c(mean = if (v == "rr") "monsum" else "monmean", max = "monmax", min = "monmin"))

  # Year filter and monthly aggregation per period
  for (p in climate_periods) {
    for (var in names(var_stats)) {
      nc_in <- file.path(climate_base, p$folder, glue::glue("{var}_ens_mean_0.1deg_reg_{p$suffix}.nc"))
      if (!file.exists(nc_in)) {
        warning(glue::glue("Not found: {nc_in} -- skipping"))
        next
      }

      stats <- var_stats[[var]]
      for (stat_label in names(stats)) {
        nc_out <- file.path(out_dir, glue::glue("{var}_{stat_label}_monthly_{p$tag}.nc"))
        if (file.exists(nc_out)) {
          message(glue::glue("  Exists: {basename(nc_out)}"))
          next
        }

        message(glue::glue("  {var} {stat_label} [{p$tag}] ..."))
        t0 <- Sys.time()
        status <- system(glue::glue("cdo {stats[[stat_label]]} -selyear,{year_range} {nc_in} {nc_out}"))
        elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
        if (status != 0) warning(glue::glue("CDO failed: {basename(nc_out)}")) else message(glue::glue("    {elapsed}s"))
      }
    }
  }

  # Merge the two periods into one continuous file
  message("\nMerging periods...")
  for (var in names(var_stats)) {
    for (stat_label in names(var_stats[[var]])) {
      f1 <- file.path(out_dir, glue::glue("{var}_{stat_label}_monthly_1995-2010.nc"))
      f2 <- file.path(out_dir, glue::glue("{var}_{stat_label}_monthly_2011-2022.nc"))
      f_merged <- file.path(out_dir, glue::glue("{var}_{stat_label}_monthly.nc"))

      if (file.exists(f_merged)) {
        message(glue::glue("  Exists: {basename(f_merged)}"))
        next
      }
      if (!file.exists(f1) || !file.exists(f2)) {
        warning(glue::glue("Missing period file for {var} {stat_label}"))
        next
      }

      message(glue::glue("  {var} {stat_label}..."))
      status <- system(glue::glue("cdo mergetime {f1} {f2} {f_merged}"))
      if (status != 0) warning(glue::glue("Merge failed: {basename(f_merged)}"))
    }
  }
}

# Monthly aggregation and period merge in pure R with terra
preprocess_climate_windows <- function(climate_base, out_dir, years) {
  out_dir <- normalizePath(out_dir, mustWork = FALSE)
  var_stats <- climate_vars |>
    purrr::set_names() |>
    purrr::map(\(v) c(mean = if (v == "rr") "sum" else "mean", max = "max", min = "min"))

  # Year filter and monthly aggregation per period
  for (p in climate_periods) {
    for (var in names(var_stats)) {
      nc_in <- file.path(climate_base, p$folder, glue::glue("{var}_ens_mean_0.1deg_reg_{p$suffix}.nc"))
      if (!file.exists(nc_in)) {
        warning(glue::glue("Not found: {nc_in} -- skipping"))
        next
      }

      message(glue::glue("\nLoading {var} [{p$tag}]..."))
      r <- terra::rast(nc_in)
      time_vals <- terra::time(r)

      # Filter to target years
      keep <- which(as.integer(format(time_vals, "%Y")) %in% years)
      if (length(keep) == 0) {
        warning(glue::glue("No layers in year range for {var} [{p$tag}]"))
        next
      }
      r <- terra::subset(r, keep)
      time_vals <- terra::time(r)
      message(glue::glue("  {terra::nlyr(r)} daily layers after year filter"))

      # Year-month index for tapp grouping
      ym_index <- format(time_vals, "%Y_%m")

      stats <- var_stats[[var]]
      for (stat_label in names(stats)) {
        nc_out <- file.path(out_dir, glue::glue("{var}_{stat_label}_monthly_{p$tag}.nc"))
        if (file.exists(nc_out)) {
          message(glue::glue("  Exists: {basename(nc_out)}"))
          next
        }

        tapp_fun <- stats[[stat_label]]
        message(glue::glue("  {var} {stat_label} ({tapp_fun}) [{p$tag}]..."))
        t0 <- Sys.time()

        r_monthly <- terra::tapp(r, ym_index, fun = tapp_fun, na.rm = TRUE)

        # Set proper time dimension from layer names
        ym_unique <- sort(unique(ym_index))
        terra::time(r_monthly) <- as.Date(paste0(gsub("_", "-", ym_unique), "-01"))
        terra::writeCDF(r_monthly, nc_out, overwrite = TRUE)

        elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
        message(glue::glue("    {terra::nlyr(r_monthly)} months -> {basename(nc_out)} ({elapsed}s)"))
        rm(r_monthly)
        gc()
      }

      rm(r)
      gc()
    }
  }

  # Merge the two periods into one continuous file
  message("\nMerging periods...")
  for (var in names(var_stats)) {
    for (stat_label in names(var_stats[[var]])) {
      f1 <- file.path(out_dir, glue::glue("{var}_{stat_label}_monthly_1995-2010.nc"))
      f2 <- file.path(out_dir, glue::glue("{var}_{stat_label}_monthly_2011-2022.nc"))
      f_merged <- file.path(out_dir, glue::glue("{var}_{stat_label}_monthly.nc"))

      if (file.exists(f_merged)) {
        message(glue::glue("  Exists: {basename(f_merged)}"))
        next
      }
      if (!file.exists(f1) || !file.exists(f2)) {
        warning(glue::glue("Missing period file for {var} {stat_label}"))
        next
      }

      message(glue::glue("  {var} {stat_label}..."))
      r1 <- terra::rast(f1)
      r2 <- terra::rast(f2)
      r_merged <- c(r1, r2)

      # Preserve time from both
      terra::time(r_merged) <- c(terra::time(r1), terra::time(r2))
      terra::writeCDF(r_merged, f_merged, overwrite = TRUE)

      message(glue::glue("    {terra::nlyr(r_merged)} months -> {basename(f_merged)}"))
      rm(r1, r2, r_merged)
      gc()
    }
  }
}
