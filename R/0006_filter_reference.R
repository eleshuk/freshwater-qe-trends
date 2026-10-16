# Filter the reference table to the year and water body combinations in joined_bqe_ref_w2010_final.parquet
# Writes parquet_tables_cleaned/swb_reference_filtered, which the spatial pipeline uses to remove coastal waters

# Keep reference rows that have BQE data, stopping if any cycle is missing reference rows
filter_ref <- function(wfd_root) {
  reference <- arrow::read_parquet(file.path(wfd_root, "parquet_tables_cleaned", "swb_reference", "part-0.parquet"))
  bqe_filtered <- arrow::read_parquet(file.path(wfd_root, "parquet_tables_joined", "joined_bqe_ref_w2010_final.parquet"))

  df_filtered <- dplyr::semi_join(reference, bqe_filtered, by = c("cYear", "euSurfaceWaterBodyCode"))

  # Compare distinct water bodies per cycle - October 2026 run: 23,358 2010, 61,787 2016, 86,805 2022
  counts <- dplyr::full_join(
    df_filtered |> dplyr::distinct(cYear, euSurfaceWaterBodyCode) |> dplyr::count(cYear, name = "n_ref"),
    bqe_filtered |> dplyr::distinct(cYear, euSurfaceWaterBodyCode) |> dplyr::count(cYear, name = "n_bqe"),
    by = "cYear"
  )
  print(counts)

  # Stop before writing if any cycle is missing reference rows
  if (any(is.na(counts$n_ref) | counts$n_ref != counts$n_bqe)) {
    stop("Reference rows missing for some BQE water bodies, nothing written")
  }

  # Write once, after every cycle has matched
  out_dir <- file.path(wfd_root, "parquet_tables_cleaned", "swb_reference_filtered")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  arrow::write_parquet(df_filtered, file.path(out_dir, "part-0.parquet"))
  invisible(df_filtered)
}
