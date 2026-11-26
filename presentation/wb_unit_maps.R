# Functions for presentation figures that place one water body inside a reporting or other spatial unit
# (River Basin District, sub-unit, biogeographical region), optionally with its CCM2 basin on top
# Uses pres_cols, pres_font, sql_quote and read_wb_context, so source theme_presentation.R and wb_context_maps.R first

# Name of the first layer in a GeoPackage matching a pattern; lists the layers if none match
find_layer <- function(gpkg, pattern) {
  layers <- sf::st_layers(gpkg)$name
  hit <- grep(pattern, layers, ignore.case = TRUE, value = TRUE)
  if (length(hit) == 0) stop("No layer matching '", pattern, "' in ", gpkg, ". Layers: ", paste(layers, collapse = ", "))
  hit[1]
}

# RBD or sub-unit code and name reported for a water body in one cycle
wb_reported_unit <- function(wb_code, cycle = 2022, level = c("rbd", "subunit"),
                             swb_path = "data/WFD/parquet_tables/swb_surface_water_body/part-0.parquet") {
  level <- match.arg(level)
  code_col <- if (level == "rbd") "euRBDCode" else "euSubUnitCode"
  name_col <- if (level == "rbd") "rbdName" else "subUnitName"

  arrow::read_parquet(swb_path, col_select = dplyr::all_of(c("cYear", "euSurfaceWaterBodyCode", code_col, name_col))) |>
    dplyr::filter(euSurfaceWaterBodyCode == wb_code, cYear == cycle) |>
    dplyr::distinct(code = .data[[code_col]], name = .data[[name_col]])
}

# RBD or sub-unit polygon for a water body, from the WISE reference GeoPackage of the same cycle, in EPSG:3035
read_wfd_unit <- function(wb_code, cycle = 2022, level = c("rbd", "subunit"),
                          spatial_dir = "data/WFD/spatial_data",
                          swb_path = "data/WFD/parquet_tables/swb_surface_water_body/part-0.parquet") {
  level <- match.arg(level)
  unit <- wb_reported_unit(wb_code, cycle, level, swb_path)
  if (nrow(unit) != 1) stop("Expected one ", level, " for ", wb_code, " in ", cycle, ", found ", nrow(unit))

  # Find this cycle's GeoPackage and its RBD or sub-unit layer
  gpkg <- list.files(spatial_dir, pattern = glue::glue("wise-wfd{cycle}_.*\\.gpkg$"), full.names = TRUE)
  layer <- find_layer(gpkg, if (level == "rbd") "RiverBasinDistrict" else "SubUnit")

  # Read only the matching feature; WISE spatial layers key features on thematicIdIdentifier
  sf::st_read(gpkg, query = glue::glue("SELECT * FROM {layer} WHERE thematicIdIdentifier = {sql_quote(unit$code)}"), quiet = TRUE) |>
    sf::st_transform(3035) |>
    dplyr::mutate(unit_code = unit$code, unit_name = unit$name)
}

# Biogeographical region containing the centre of the water body's catchment, in EPSG:3035
read_biogeo_unit <- function(ctx, biogeo_path = "data/biogeographical_regions/BiogeoRegions2016.shp",
                             region_col = "short_name") {
  biogeo <- sf::st_read(biogeo_path, quiet = TRUE)
  centre <- sf::st_centroid(sf::st_union(ctx$catchments)) |>
    sf::st_transform(sf::st_crs(biogeo))

  biogeo[lengths(sf::st_intersects(biogeo, centre)) > 0, ] |>
    sf::st_transform(3035) |>
    dplyr::mutate(unit_code = .data[[region_col]], unit_name = .data[[region_col]])
}

# One map of the water body inside a unit; with show_basin the CCM2 basin is drawn dashed on top
# ctx comes from read_wb_context(); view sets the extent (defaults to the unit, plus the basin when shown)
plot_wb_in_unit <- function(ctx, unit, title, subtitle, caption = NULL, view = NULL, show_basin = TRUE,
                            europe_path = "data/drivers/ne_50m_europe.gpkg", pad = 0.08,
                            out_file = NULL, width = 6, height = 6, font = pres_font) {
  catchment_outline <- sf::st_union(ctx$catchments)
  marker <- sf::st_centroid(catchment_outline)

  # Map extent from the bounding box of the view geometry, padded on every side
  if (is.null(view)) {
    view <- sf::st_geometry(unit)
    if (show_basin) view <- c(view, sf::st_geometry(ctx$basin))
  }
  bb <- sf::st_bbox(view)
  dx <- unname(bb["xmax"] - bb["xmin"]) * pad
  dy <- unname(bb["ymax"] - bb["ymin"]) * pad
  xlim <- unname(c(bb["xmin"] - dx, bb["xmax"] + dx))
  ylim <- unname(c(bb["ymin"] - dy, bb["ymax"] + dy))

  # Country outlines so national borders show where the unit stops
  europe <- sf::st_read(europe_path, quiet = TRUE) |>
    sf::st_transform(3035)

  # Same map styling as plot_wb_context()
  map_theme <- theme_void(base_family = font) +
    theme(
      plot.background = element_rect(fill = pres_cols[["background"]], colour = NA),
      plot.title = element_text(face = "bold", hjust = 0.5, colour = pres_cols[["dark_green"]]),
      plot.subtitle = element_text(hjust = 0.5, colour = pres_cols[["green"]]),
      plot.caption = element_text(hjust = 0.5, colour = pres_cols[["green"]], size = 8),
      plot.margin = margin(10, 10, 10, 10)
    )

  # Layers from back to front: countries, unit, basin outline, catchment, water body, marker
  p <- ggplot() +
    geom_sf(data = europe, fill = pres_cols[["land"]], colour = pres_cols[["land_border"]], linewidth = 0.2) +
    geom_sf(data = unit, fill = pres_cols[["pale_green"]], colour = pres_cols[["green"]], linewidth = 0.5) +
    (if (show_basin) geom_sf(data = ctx$basin, fill = NA, colour = pres_cols[["dark_tan"]], linewidth = 0.5, linetype = "dashed")) +
    geom_sf(data = catchment_outline, fill = pres_cols[["tan"]], colour = pres_cols[["dark_tan"]], linewidth = 0.3) +
    geom_sf(data = ctx$wb, colour = pres_cols[["dark_green"]], fill = pres_cols[["dark_green"]], linewidth = 0.4) +
    geom_sf(data = marker, shape = 21, size = 8, stroke = 1, colour = pres_cols[["dark_green"]], fill = NA) +
    coord_sf(xlim = xlim, ylim = ylim, crs = 3035, expand = FALSE) +
    labs(title = title, subtitle = subtitle, caption = caption) +
    map_theme

  if (!is.null(out_file)) {
    ggsave(out_file, p, width = width, height = height, dpi = 300, bg = pres_cols[["background"]])
  }

  invisible(p)
}
