# Create view merged on the BQE table and the spatial data
# Inner joins to only get spatial data that has BQE data
# Each year's spatial data is read, joined, and written before the next one
# is loaded, so you never have all three years of geometry in memory at once

# Joins spatial data to BQE data, one file per cycle
join_bqe_spatial_data <- function(yr, wfd_root) {
  message("Processing ", yr, "...")
  
  spatial_root <- file.path(wfd_root, "parquet_tables_spatial")
  
  # Read spatial files for the given year
  swb <- sfarrow::st_read_parquet(
    glue::glue("{spatial_root}/surface_water_bodies/{yr}_spatial_data_swb.parquet")
  )
  rivers <- sfarrow::st_read_parquet(
    glue::glue("{spatial_root}/rivers/{yr}_spatial_data_rivers.parquet")
  )
  
  # Summarise BQE data to one row per water body
  bqe_summary <- arrow::read_parquet(
    file.path(wfd_root, "parquet_tables_joined", "joined_bqe_ref_w2010_final.parquet")
  ) |>
    dplyr::filter(cYear == as.integer(yr)) |>
    dplyr::summarise(
      n_bqe = dplyr::n_distinct(qeCode),
      dplyr::across(
        c(cYear, countryCode, countryName, rbdName, swEcologicalStatusOrPotentialValue,
          swChemicalStatusValue, surfaceWaterBodyName, wiseEvolutionType,
          surfaceWaterBodyCategory, qeStatusOrPotentialValueLabel),
        dplyr::first
      ),
      .by = euSurfaceWaterBodyCode
    ) |>
    dplyr::arrange(dplyr::desc(n_bqe)) |>
    dplyr::mutate(cYear = as.integer(cYear))
  
  # Drop geometry before joining so dplyr operates on plain scalars
  swb_no_geom <- swb |>
    sf::st_drop_geometry() |>
    dplyr::select(cYear, thematicIdIdentifier, sizeValue, sizeUom, meanDepth, lon, lat)
  
  rivers_no_geom <- rivers |>
    sf::st_drop_geometry() |>
    dplyr::select(cYear, thematicIdIdentifier, sizeValue, sizeUom, meanDepth, lon, lat)
  
  # Join BQE summary to attribute columns
  swb_joined <- bqe_summary |>
    dplyr::inner_join(swb_no_geom, by = dplyr::join_by(cYear, euSurfaceWaterBodyCode == thematicIdIdentifier))
  
  rivers_joined <- bqe_summary |>
    dplyr::inner_join(rivers_no_geom, by = dplyr::join_by(cYear, euSurfaceWaterBodyCode == thematicIdIdentifier))
  
  # Pull geometry only for matched rows then re-attach
  swb_geom <- swb |>
    dplyr::select(cYear, thematicIdIdentifier, geom) |>
    dplyr::filter(thematicIdIdentifier %in% swb_joined$euSurfaceWaterBodyCode)
  
  rivers_geom <- rivers |>
    dplyr::select(cYear, thematicIdIdentifier, geom) |>
    dplyr::filter(thematicIdIdentifier %in% rivers_joined$euSurfaceWaterBodyCode)
  
  bqe_polygons <- swb_joined |>
    dplyr::left_join(swb_geom, by = dplyr::join_by(cYear, euSurfaceWaterBodyCode == thematicIdIdentifier)) |>
    sf::st_as_sf() |>
    dplyr::mutate(geom_type = "polygon")
  
  bqe_rivers <- rivers_joined |>
    dplyr::left_join(rivers_geom, by = dplyr::join_by(cYear, euSurfaceWaterBodyCode == thematicIdIdentifier)) |>
    sf::st_as_sf() |>
    dplyr::mutate(geom_type = "linestring")
  
  combined <- dplyr::bind_rows(bqe_polygons, bqe_rivers)
  
  message(yr, ": ", dplyr::n_distinct(combined$euSurfaceWaterBodyCode), " unique WBs")
  
  sfarrow::st_write_parquet(
    combined,
    glue::glue("{spatial_root}/bqe_spatial_joined_{yr}.parquet")
  )
}
