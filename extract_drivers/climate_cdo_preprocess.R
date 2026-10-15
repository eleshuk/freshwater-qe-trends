# CDO preprocessing for E-OBS climate data
# Filters daily NetCDFs to 2004-2022, aggregates to monthly
# mean/max/min, and merges the two source periods into one
# continuous file per variable and statistic.
#
# Requires CDO (Climate Data Operators).
# Install: brew install cdo (macOS) or apt install cdo (Linux)
#
# Run this once before climate_catchment_extraction.R.
# Working directory: Spatial_workflow_work/

library(glue)

# root <- "/Users/eleshuk/Documents/ISA_class_materials/Thesis/"
root <- "/Users/isa128539/Desktop/"
# climate_base <- paste0(root, "data/climate/")
climate_base <- paste0(root, "driver_data/climate/")
out_dir <- paste0(climate_base, "outputs/")
year_range <- "2004/2022"

periods <- list(
  list(folder = "1995-2010_climate", suffix = "1995-2010_v33.0e", tag = "1995-2010"),
  list(folder = "2011-2022_climate", suffix = "2011-2022_v27.0e", tag = "2011-2022")
  
)

# Per-variable stats: rainfall uses monsum for central tendency, others use monmean
var_stats <- list(
  tg = c(mean = "monmean", max = "monmax", min = "monmin"),
  tn = c(mean = "monmean", max = "monmax", min = "monmin"),
  tx = c(mean = "monmean", max = "monmax", min = "monmin"),
  rr = c(mean = "monsum",  max = "monmax", min = "monmin"),
  hu = c(mean = "monmean", max = "monmax", min = "monmin")
)

# Check CDO is available
if (system("cdo --version", ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
  stop("CDO not found. Install with: brew install cdo on Mac or ")
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Step 1: year-filter + monthly aggregation per period
for (p in periods) {
  for (var in names(var_stats)) {
    nc_in <- file.path(climate_base, p$folder,
                       glue("{var}_ens_mean_0.1deg_reg_{p$suffix}.nc"))
    if (!file.exists(nc_in)) {
      warning(glue("Not found: {nc_in} -- skipping"))
      next
    }
    
    stats <- var_stats[[var]]
    for (stat_label in names(stats)) {
      cdo_op <- stats[[stat_label]]
      nc_out <- file.path(out_dir,
                          glue("{var}_{stat_label}_monthly_{p$tag}.nc"))
      
      if (file.exists(nc_out)) {
        message(glue("  Exists: {basename(nc_out)}"))
        next
      }
      
      cmd <- glue("cdo {cdo_op} -selyear,{year_range} {nc_in} {nc_out}")
      message(glue("  {var} {stat_label} [{p$tag}] ..."))
      t0 <- Sys.time()
      status <- system(cmd)
      elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
      
      if (status != 0) warning(glue("CDO failed: {basename(nc_out)}"))
      else message(glue("    {elapsed}s"))
    }
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
    
    cmd <- glue("cdo mergetime {f1} {f2} {f_merged}")
    message(glue("  {var} {stat_label}..."))
    status <- system(cmd)
    if (status != 0) warning(glue("Merge failed: {basename(f_merged)}"))
  }
}

message(glue("\nDone. Monthly files in: {out_dir}"))

