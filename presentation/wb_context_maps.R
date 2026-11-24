# Functions for presentation figures that place one water body in its catchment, basin and Europe
# Uses pres_cols and pres_font from presentation/theme_presentation.R, so source that file first
# Inputs are the same files the pipeline uses; every layer is read with a filter, so only the features needed are loaded

# Quote a value for use inside an SQL string
sql_quote <- function(x) {
  paste0("'", gsub("'", "''", x), "'")
}

# River water bodies in one CCM2 basin that have a quality element assessed, ranked by number of cycles
find_basin_wbs <- function(basin_pattern, qe_code = "QE1-4", category = "RW",
                           gdb_path = "data/spatial/CCM2_stuff.gdb",
                           workflow_gpkg = "data/spatial/Workflow_outputs.gpkg",
                           joined_path = "data/WFD/parquet_tables_joined/joined_bqe_ref_w2010_final.parquet") {
  # Find the basin in the CCM2 basin layer
  basins <- sf::st_read(gdb_path, layer = "EU_basins", quiet = TRUE) |>
    sf::st_drop_geometry() |>
    dplyr::filter(grepl(basin_pattern, NAME, ignore.case = TRUE))

  # Read only the catchment-to-basin attributes, without geometry
  pc_basin <- sf::st_read(gdb_path, query = "SELECT WSO1_ID, WSO_ID FROM EU_primary_catchments", quiet = TRUE) |>
    sf::st_drop_geometry()

  # Water bodies whose linked catchments fall in the basin
  wb_basin <- sf::st_read(workflow_gpkg, layer = "sWB_CCM_tbl", quiet = TRUE) |>
    sf::st_drop_geometry() |>
    dplyr::inner_join(pc_basin, by = "WSO1_ID") |>
    dplyr::filter(WSO_ID %in% basins$WSO_ID) |>
    dplyr::distinct(WB_code)

  # Water bodies with the quality element assessed, ranked by number of cycles
  # qeCode holds the full label (e.g. "QE1-4 - Fish"); qe_code can be the full label or only the code ("QE1-4")
  arrow::read_parquet(
    joined_path,
    col_select = c("cYear", "euSurfaceWaterBodyCode", "qeCode", "surfaceWaterBodyName", "countryCode", "surfaceWaterBodyCategory")
  ) |>
    dplyr::filter(
      euSurfaceWaterBodyCode %in% wb_basin$WB_code,
      qeCode == qe_code | sub(" - .*$", "", qeCode) == qe_code,
      surfaceWaterBodyCategory == category
    ) |>
    dplyr::count(euSurfaceWaterBodyCode, surfaceWaterBodyName, countryCode, name = "n_cycles") |>
    dplyr::arrange(dplyr::desc(n_cycles))
}

# Read the water body, its linked CCM2 primary catchments and the basin it overlaps most, all in EPSG:3035
read_wb_context <- function(wb_code,
                            input_gpkg = "data/spatial/ccm2_wfd_join/Input_data_filtered.gpkg",
                            workflow_gpkg = "data/spatial/Workflow_outputs.gpkg",
                            gdb_path = "data/spatial/CCM2_stuff.gdb") {
  # Water body geometry from the polygon and line layers; one of them is usually empty
  wb <- c("SurfaceWaterBody", "SurfaceWaterBodyLine") |>
    purrr::map(\(layer) {
      sf::st_read(input_gpkg, query = glue::glue("SELECT * FROM {layer} WHERE WB_code = {sql_quote(wb_code)}"), quiet = TRUE) |>
        sf::st_transform(3035) |>
        dplyr::select(WB_code)
    }) |>
    dplyr::bind_rows()
  if (nrow(wb) == 0) stop("Water body ", wb_code, " not found in ", input_gpkg)

  # Catchments linked to the water body, with the overlap used to pick its basin (as in R/0305_merge_rbs.R)
  links <- sf::st_read(
    workflow_gpkg,
    query = glue::glue("SELECT * FROM sWB_CCM_tbl WHERE WB_code = {sql_quote(wb_code)}"),
    quiet = TRUE
  ) |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(WSO1_ID)) |>
    dplyr::mutate(overlap = dplyr::coalesce(WB_CCM2_area, WB_CCM2_length, 0)) |>
    dplyr::select(WSO1_ID, overlap)
  if (nrow(links) == 0) stop("Water body ", wb_code, " has no linked catchments in ", workflow_gpkg)
  id_list <- paste(unique(links$WSO1_ID), collapse = ", ")

  # Primary catchment polygons
  catchments <- sf::st_read(
    input_gpkg,
    query = glue::glue("SELECT * FROM EU_primary_catchments WHERE WSO1_ID IN ({id_list})"),
    quiet = TRUE
  ) |>
    sf::st_transform(3035) |>
    dplyr::select(WSO1_ID)

  # Basin with the largest overlap; ties go to the lowest WSO_ID
  basin_id <- sf::st_read(
    gdb_path,
    query = glue::glue("SELECT WSO1_ID, WSO_ID FROM EU_primary_catchments WHERE WSO1_ID IN ({id_list})"),
    quiet = TRUE
  ) |>
    sf::st_drop_geometry() |>
    dplyr::inner_join(links, by = "WSO1_ID") |>
    dplyr::summarise(overlap = sum(overlap), .by = WSO_ID) |>
    dplyr::arrange(dplyr::desc(overlap), WSO_ID) |>
    dplyr::pull(WSO_ID) |>
    dplyr::first()

  # Basin polygon
  basin <- sf::st_read(gdb_path, query = glue::glue("SELECT * FROM EU_basins WHERE WSO_ID = {basin_id}"), quiet = TRUE) |>
    sf::st_transform(3035)

  list(wb = wb, catchments = catchments, basin = basin)
}

# Four maps of one water body: on its own, in its catchment, in its basin, and in Europe
# Returns a named list of ggplots; with out_dir set, each is also saved as a PNG
plot_wb_context <- function(wb_code, title = wb_code,
                            input_gpkg = "data/spatial/ccm2_wfd_join/Input_data_filtered.gpkg",
                            workflow_gpkg = "data/spatial/Workflow_outputs.gpkg",
                            gdb_path = "data/spatial/CCM2_stuff.gdb",
                            europe_path = "data/drivers/ne_50m_europe.gpkg",
                            out_dir = NULL, width = 6, height = 6, font = pres_font) {
  ctx <- read_wb_context(wb_code, input_gpkg, workflow_gpkg, gdb_path)

  # Colours shared by all four maps, from the slide palette
  wb_col <- pres_cols[["dark_green"]]
  catch_fill <- pres_cols[["tan"]]
  catch_col <- pres_cols[["dark_tan"]]
  basin_fill <- pres_cols[["pale_green"]]
  basin_col <- pres_cols[["green"]]

  # Catchment outline with the internal primary catchment boundaries removed
  catchment_outline <- sf::st_union(ctx$catchments)

  # Water body layer drawn on top of every map; lines get a colour, polygons a fill
  wb_layer <- function(linewidth) {
    geom_sf(data = ctx$wb, colour = wb_col, fill = wb_col, linewidth = linewidth)
  }

  # Shared map styling: slide background, Montserrat titles in the slide greens
  map_theme <- theme_void(base_family = font) +
    theme(
      plot.background = element_rect(fill = pres_cols[["background"]], colour = NA),
      plot.title = element_text(face = "bold", hjust = 0.5, colour = pres_cols[["dark_green"]]),
      plot.subtitle = element_text(hjust = 0.5, colour = pres_cols[["green"]]),
      plot.margin = margin(10, 10, 10, 10)
    )

  # 1) The water body on its own
  p_wb <- ggplot() +
    wb_layer(1) +
    labs(title = title, subtitle = "Water body") +
    map_theme

  # 2) The water body in its CCM2 primary catchments
  p_catchment <- ggplot() +
    geom_sf(data = ctx$catchments, fill = catch_fill, colour = pres_cols[["background"]], linewidth = 0.3) +
    geom_sf(data = catchment_outline, fill = NA, colour = catch_col, linewidth = 0.6) +
    wb_layer(0.9) +
    labs(title = title, subtitle = glue::glue("Catchment ({nrow(ctx$catchments)} CCM2 primary catchments)")) +
    map_theme

  # 3) The water body and catchment in the basin
  p_basin <- ggplot() +
    geom_sf(data = ctx$basin, fill = basin_fill, colour = basin_col, linewidth = 0.4) +
    geom_sf(data = catchment_outline, fill = catch_fill, colour = catch_col, linewidth = 0.3) +
    wb_layer(0.5) +
    labs(title = title, subtitle = "River basin (CCM2)") +
    map_theme

  # 4) Europe, with a marker so the catchment is visible at this scale; extent matches the thesis maps
  europe <- sf::st_read(europe_path, quiet = TRUE) |>
    sf::st_transform(3035)
  marker <- sf::st_centroid(catchment_outline)
  p_europe <- ggplot() +
    geom_sf(data = europe, fill = pres_cols[["land"]], colour = pres_cols[["land_border"]], linewidth = 0.2) +
    geom_sf(data = ctx$basin, fill = NA, colour = basin_col, linewidth = 0.3) +
    geom_sf(data = catchment_outline, fill = catch_col, colour = catch_col, linewidth = 0.3) +
    geom_sf(data = marker, shape = 21, size = 5, stroke = 1, colour = pres_cols[["dark_green"]], fill = NA) +
    coord_sf(xlim = c(2000000, 6500000), ylim = c(1300000, 5500000), crs = 3035, expand = FALSE) +
    labs(title = title, subtitle = "Location in Europe") +
    map_theme

  plots <- list(water_body = p_wb, catchment = p_catchment, basin = p_basin, europe = p_europe)

  # Save each map as <wb_code>_<n>_<name>.png
  if (!is.null(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    purrr::iwalk(plots, \(p, name) {
      n <- match(name, names(plots))
      ggsave(file.path(out_dir, glue::glue("{wb_code}_{n}_{name}.png")), p, width = width, height = height, dpi = 300, bg = pres_cols[["background"]])
    })
  }

  invisible(plots)
}
