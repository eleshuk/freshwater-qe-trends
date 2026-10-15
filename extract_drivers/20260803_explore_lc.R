library(terra)

# clc_dir <- "/Users/eleshuk/Documents/ISA_class_materials/Thesis/data/land_cover/Results/"
root <- "/Users/isa128539/Desktop/"


clc_files <- c(
  "2006" = file.path(clc_dir, "u2012_clc2006_v2020_20u1_raster100m/DATA/U2012_CLC2006_V2020_20u1.tif"),
  "2012" = file.path(clc_dir, "u2018_clc2012_v2020_20u1_raster100m/DATA/U2018_CLC2012_V2020_20u1.tif"),
  "2018" = file.path(clc_dir, "u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")
)

# Load all three (lazy — doesn't read into memory)
clc <- lapply(clc_files, rast)

# Basic metadata
lapply(clc, print)

# Check CRS, resolution, extent match
sapply(clc, res)
sapply(clc, ext)
sapply(clc, crs, proj = TRUE)

# Value frequency table for each year (uses the built-in VAT)
freq_tables <- lapply(clc, freq)
freq_tables

# Quick plot of one year to sanity-check
plot(clc[["2018"]], main = "CLC 2018")

# Each pixel is 100m × 100m = 10,000 m² = 1 hectare
freq_df <- lapply(names(freq_tables), \(yr) {
  freq_tables[[yr]] |>
    dplyr::mutate(year = yr)
}) |>
  dplyr::bind_rows()

# Ready to join to spatial data


