# Create views to be used for analysis
library(arrow)
library(sfarrow)
library(dplyr)
library(ggplot2)

# Load filter qe table
wfd_root <- "/Users/eleshuk/Documents/ISA_class_materials/Thesis/data/WFD/"

join_bqe_nogeo <- function() {
  # Read and combine all spatial parquet tables, dropping geometry
  bqe_data <- purrr::map(
    list.files(
      "/Users/eleshuk/Documents/ISA_class_materials/Thesis/data/WFD/parquet_tables_spatial/",
      pattern = "bqe_spatial_joined_",
      full.names = TRUE
    ),
    \(f) sfarrow::st_read_parquet(f) |> sf::st_drop_geometry()
  ) |>
    purrr::list_rbind()
  
  # Filter to only include BQEs matching cYear, euSurfaceWaterBodyCode, and qeCode from ref
  bqe_ref <- arrow::read_parquet(
    "/Users/eleshuk/Documents/ISA_class_materials/Thesis/data/WFD/parquet_tables_joined/joined_bqe_ref_w2010_final.parquet"
  )
  
  bqe_ref <- bqe_ref |>
    dplyr::mutate(cYear = as.integer(cYear))
  
  bqe_data |>
    dplyr::semi_join(bqe_ref, by = c("cYear", "euSurfaceWaterBodyCode")) |>
    arrow::write_parquet(
      "/Users/eleshuk/Documents/ISA_class_materials/Thesis/data/WFD/parquet_tables_joined/bqe_ref_nogeo.parquet"
    )
  
  bqe_data |>
    dplyr::group_by(cYear) |>
    dplyr::summarise(n_unique = dplyr::n_distinct(euSurfaceWaterBodyCode))
}

join_bqe_nogeo()
  
