# Combine the per-cycle spatial BQE tables from 0301, drop geometry and keep water bodies in the joined BQE table
# Writes parquet_tables_joined/bqe_ref_nogeo.parquet, used for lat/lon in 0304_merge_biogeography.R

# Read the spatial joins for each cycle without geometry, filter to the BQE table and write
join_bqe_nogeo <- function(wfd_root, years = c("2010", "2016", "2022")) {
  # Read only the files written by join_bqe_spatial_data(), not older suffixed versions
  bqe_data <- purrr::map(
    file.path(wfd_root, "parquet_tables_spatial", paste0("bqe_spatial_joined_", years, ".parquet")),
    \(f) sfarrow::st_read_parquet(f) |> sf::st_drop_geometry()
  ) |>
    purrr::list_rbind()

  # Filter to only include water bodies matching cYear and euSurfaceWaterBodyCode in the BQE table
  bqe_ref <- arrow::read_parquet(
    file.path(wfd_root, "parquet_tables_joined", "joined_bqe_ref_w2010_final.parquet")
  ) |>
    dplyr::mutate(cYear = as.integer(cYear))

  bqe_data |>
    dplyr::semi_join(bqe_ref, by = c("cYear", "euSurfaceWaterBodyCode")) |>
    arrow::write_parquet(file.path(wfd_root, "parquet_tables_joined", "bqe_ref_nogeo.parquet"))

  # Return water bodies per cycle as a check
  bqe_data |>
    dplyr::summarise(n_unique = dplyr::n_distinct(euSurfaceWaterBodyCode), .by = cYear)
}
