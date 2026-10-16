# Add the CCM2 river basin (WSO_ID) to the model-ready dataset and drop rows without climate drivers
# Each water body is assigned the basin it overlaps most (area for lakes, length for rivers)

# Build the water body to basin lookup, join it and write model_ready_biogeo_rb.parquet
merge_rbs <- function(workflow_gpkg, gdb_path, input_path, output_path) {
  # WB to catchment mapping, with overlap size (area for lakes, length for rivers)
  swb <- sf::st_read(workflow_gpkg, layer = "sWB_CCM_tbl", quiet = TRUE) |>
    sf::st_drop_geometry() |>
    dplyr::mutate(overlap = dplyr::coalesce(WB_CCM2_area, WB_CCM2_length, 0)) |>
    dplyr::select(euSurfaceWaterBodyCode = WB_code, WSO1_ID, overlap)

  # Catchment to basin mapping
  catch_basin <- sf::st_read(gdb_path, layer = "EU_primary_catchments",
                             query = "SELECT WSO1_ID, WSO_ID FROM EU_primary_catchments", quiet = TRUE) |>
    sf::st_drop_geometry() |>
    dplyr::distinct()

  # Total overlap of each water body with each basin
  wb_basin_overlap <- swb |>
    dplyr::left_join(catch_basin, by = "WSO1_ID", relationship = "many-to-one") |>
    dplyr::filter(!is.na(WSO_ID)) |>
    dplyr::summarise(overlap = sum(overlap), .by = c(euSurfaceWaterBodyCode, WSO_ID))

  # Report water bodies that span more than one basin
  n_multi_basin <- wb_basin_overlap |>
    dplyr::count(euSurfaceWaterBodyCode) |>
    dplyr::filter(n > 1) |>
    nrow()
  cat("Water bodies in more than one basin:", n_multi_basin, "\n")

  # Keep the basin with the largest overlap; ties go to the lowest WSO_ID so reruns give the same answer
  rbd_lookup <- wb_basin_overlap |>
    dplyr::arrange(euSurfaceWaterBodyCode, dplyr::desc(overlap), WSO_ID) |>
    dplyr::distinct(euSurfaceWaterBodyCode, .keep_all = TRUE) |>
    dplyr::select(euSurfaceWaterBodyCode, ccm2_basin = WSO_ID)
  cat("Unique basins:", dplyr::n_distinct(rbd_lookup$ccm2_basin, na.rm = TRUE), "\n")

  # Join basins and drop rows without climate drivers
  df_raw <- arrow::read_parquet(input_path) |>
    dplyr::left_join(rbd_lookup, by = "euSurfaceWaterBodyCode", relationship = "many-to-one")
  df <- df_raw |>
    dplyr::filter(!is.na(bio01))

  n_wb_raw <- dplyr::n_distinct(df_raw$euSurfaceWaterBodyCode)
  n_wb <- dplyr::n_distinct(df$euSurfaceWaterBodyCode)
  cat("Unique WBs before:", n_wb_raw, "\n")
  cat("Unique WBs after:", n_wb, "\n")
  cat("Unique WBs dropped:", n_wb_raw - n_wb, "\n")
  cat("Rows before:", nrow(df_raw), "\n")
  cat("Rows after:", nrow(df), "\n")
  cat("Basin NAs:", sum(is.na(df$ccm2_basin)), "\n")

  # Write output
  arrow::write_parquet(df, output_path)
  invisible(df)
}
