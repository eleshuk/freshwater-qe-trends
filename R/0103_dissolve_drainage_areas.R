# Dissolve the CCM2 primary catchments linked to each water body into one drainage area per water body
# Links come from the sWB_CCM_tbl layer written by python/0102_assign_catchments.py, catchment
# geometries from the EU_primary_catchments layer of Input_data_filtered.gpkg
# Writes WB_drainage_areas.gpkg (WB_code, n_catchments, geometry), read by R/0304_merge_biogeography.R

# Build and write the drainage area of every water body linked to at least one catchment
dissolve_drainage_areas <- function(workflow_gpkg = "data/spatial/Workflow_outputs.gpkg",
                                    catchment_gpkg = "data/spatial/ccm2_wfd_join/Input_data_filtered.gpkg",
                                    output_gpkg = "data/biogeographical_regions/WB_drainage_areas.gpkg") {
  # Water body to catchment links; network-only rows have no WB_code and are dropped
  links <- sf::st_read(workflow_gpkg, layer = "sWB_CCM_tbl", quiet = TRUE) |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(WB_code), !is.na(WSO1_ID)) |>
    dplyr::distinct(WB_code, WSO1_ID)
  cat("Links:", nrow(links), "| water bodies:", dplyr::n_distinct(links$WB_code),
      "| catchments:", dplyr::n_distinct(links$WSO1_ID), "\n")

  # Catchment geometries, keeping only the linked ones
  catchments <- sf::st_read(catchment_gpkg, layer = "EU_primary_catchments", quiet = TRUE) |>
    dplyr::select(WSO1_ID) |>
    dplyr::filter(WSO1_ID %in% links$WSO1_ID)

  # A catchment can drain to several water bodies, so each one is copied to every water body it is linked to
  cat("Dissolving catchments per water body...\n")
  drainage <- catchments |>
    dplyr::inner_join(links, by = "WSO1_ID", relationship = "one-to-many") |>
    dplyr::group_by(WB_code) |>
    dplyr::summarise(n_catchments = dplyr::n()) |>
    sf::st_make_valid()
  cat("Drainage areas:", nrow(drainage), "\n")

  # Start fresh and write
  if (file.exists(output_gpkg)) file.remove(output_gpkg)
  dir.create(dirname(output_gpkg), showWarnings = FALSE, recursive = TRUE)
  sf::st_write(drainage, output_gpkg, layer = "WB_drainage_areas", quiet = TRUE)

  invisible(drainage)
}
