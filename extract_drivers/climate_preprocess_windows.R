# Climate preprocessing (pure R, no CDO - for Windows)
# Filters daily E-OBS NetCDFs to 2004-2022, aggregates to monthly
# mean/max/min (sum for rainfall), and merges the two source periods
# into one continuous file per variable and statistic.
#
# This is the Windows-compatible alternative to climate_cdo_preprocess.R.
# Slower than CDO (~1-1.5 min per aggregation vs ~4 seconds) but has
# no external dependencies beyond R packages.
#
# Run this once before climate_catchment_extraction.R.

library(terra)
library(glue)
library(ncdf4)

# --- Paths ---
root <- "/Users/isa128539/Desktop/"
# root <- "/Users/eleshuk/Documents/ISA_class_materials/Thesis/"

climate_base <- paste0(root, "drivers_data/climate/")
# climate_base <- paste0(root, "data/climate/")

out_dir <- paste0(climate_base, "outputs/")
out_dir <- normalizePath(out_dir, mustWork = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

year_range <- 2004:2022

periods <- list(
  list(folder = "1995-2010_climate", suffix = "1995-2010_v33.0e", tag = "1995-2010"),
  list(folder = "2011-2022_climate", suffix = "2011-2022_v27.0e", tag = "2011-2022")
)

# terra::tapp function names for each stat
# Central tendency uses "mean" for temp/humidity, "sum" for rainfall
var_stats <- list(
  tg = c(mean = "mean", max = "max", min = "min"),
  tn = c(mean = "mean", max = "max", min = "min"),
  tx = c(mean = "mean", max = "max", min = "min"),
  rr = c(mean = "sum",  max = "max", min = "min"),
  hu = c(mean = "mean", max = "max", min = "min")
)

# Step 1: filter years + monthly aggregation per period
for (p in periods) {
  for (var in names(var_stats)) {
    nc_in <- file.path(climate_base, p$folder,
                       glue("{var}_ens_mean_0.1deg_reg_{p$suffix}.nc"))
    
    if (!file.exists(nc_in)) {
      warning(glue("Not found: {nc_in} -- skipping"))
      next
    }
    
    message(glue("\nLoading {var} [{p$tag}]..."))
    r <- terra::rast(nc_in)
    time_vals <- terra::time(r)
    
    # Filter to target years
    keep <- which(as.integer(format(time_vals, "%Y")) %in% year_range)
    if (length(keep) == 0) {
      warning(glue("No layers in year range for {var} [{p$tag}]"))
      next
    }
    r <- terra::subset(r, keep)
    time_vals <- terra::time(r)
    message(glue("  {terra::nlyr(r)} daily layers after year filter"))
    
    # Year-month index for tapp grouping
    ym_index <- format(time_vals, "%Y_%m")
    
    stats <- var_stats[[var]]
    for (stat_label in names(stats)) {
      nc_out <- file.path(out_dir,
                          glue("{var}_{stat_label}_monthly_{p$tag}.nc"))
      
      if (file.exists(nc_out)) {
        message(glue("  Exists: {basename(nc_out)}"))
        next
      }
      
      tapp_fun <- stats[[stat_label]]
      message(glue("  {var} {stat_label} ({tapp_fun}) [{p$tag}]..."))
      t0 <- Sys.time()
      
      r_monthly <- terra::tapp(r, ym_index, fun = tapp_fun, na.rm = TRUE)
      
      # Set proper time dimension from layer names
      ym_unique <- sort(unique(ym_index))
      layer_dates <- as.Date(paste0(gsub("_", "-", ym_unique), "-01"))
      terra::time(r_monthly) <- layer_dates
      
      terra::writeCDF(r_monthly, nc_out, overwrite = TRUE)
      
      elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
      message(glue("    {terra::nlyr(r_monthly)} months -> {basename(nc_out)} ({elapsed}s)"))
      
      rm(r_monthly)
      gc()
    }
    
    rm(r)
    gc()
  }
}

# Step 2: merge the two periods into one continuous file
message("\nMerging periods...")
for (var in names(var_stats)) {
  for (stat_label in names(var_stats[[var]])) {
    f1 <- file.path(out_dir, glue("{var}_{stat_label}_monthly_1995-2010.nc"))
    f2 <- file.path(out_dir, glue("{var}_{stat_label}_monthly_2011-2022.nc"))
    f_merged <- file.path(out_dir, glue("{var}_{stat_label}_monthly.nc"))
    
    if (file.exists(f_merged)) {
      message(glue("  Exists: {basename(f_merged)}"))
      next
    }
    if (!file.exists(f1) || !file.exists(f2)) {
      warning(glue("Missing period file for {var} {stat_label}"))
      next
    }
    
    message(glue("  {var} {stat_label}..."))
    r1 <- terra::rast(f1)
    r2 <- terra::rast(f2)
    r_merged <- c(r1, r2)
    
    # Preserve time from both
    terra::time(r_merged) <- c(terra::time(r1), terra::time(r2))
    terra::writeCDF(r_merged, f_merged, overwrite = TRUE)
    
    message(glue("    {terra::nlyr(r_merged)} months -> {basename(f_merged)}"))
    rm(r1, r2, r_merged)
    gc()
  }
}

message(glue("\nDone. Monthly files in: {out_dir}"))
