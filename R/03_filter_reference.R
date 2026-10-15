# Filter out year and swb combos that exist in joined_bqe_ref_w2010.parquet
# This will need to be done for all cleaned tables

library(arrow)
library(dplyr)
library(purrr)
library(stringr)

wfd_root <- "/Users/eleshuk/Documents/ISA_class_materials/Thesis/data/WFD/"

filter_ref <- function(reference, bqe_filtered){
  
  df_filtered <- dplyr::semi_join(reference, bqe_filtered, by = c("cYear", "euSurfaceWaterBodyCode"))
  
  # Check results - 30,774 2010, 72,207 2016, 90,486 2022
  years <- c("2010", "2016", "2022")
  for (year in years) {
    n_dist <- df_filtered |> 
      dplyr::filter(cYear == year) |> 
      dplyr::distinct(euSurfaceWaterBodyCode) |> 
      nrow()
    
    n_dist_orig <- bqe_filtered |> 
      dplyr::filter(cYear == year) |> 
      dplyr::distinct(euSurfaceWaterBodyCode) |> 
      nrow()
    
    if (n_dist == n_dist_orig) {
      cat(year, "- match:", n_dist, "\n")
      out_dir <- file.path(wfd_root, "parquet_tables_cleaned", "swb_reference_filtered")
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      arrow::write_parquet(df_filtered, file.path(out_dir, "part-0.parquet"))
    } else {
      warning(year, " - mismatch: filtered=", n_dist, " orig=", n_dist_orig)
    }
  }
}


reference <- read_parquet("/Users/eleshuk/Documents/ISA_class_materials/Thesis/data/WFD/parquet_tables_cleaned/swb_reference/part-0.parquet")
bqe_data <- read_parquet("/Users/eleshuk/Documents/ISA_class_materials/Thesis/data/WFD/parquet_tables_joined/joined_bqe_ref_w2010_final.parquet")

filter_ref(reference = reference, bqe_filtered = bqe_data)
