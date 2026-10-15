# ============================================================
# Add biogeographical region to model_ready dataset
# Pass 1: WB_drainage_areas centroids (91k WBs)
# Pass 2: WB lat/lon fallback for unmatched WBs
# NO water bodies are dropped
# ============================================================
library(sf)
library(dplyr)
library(arrow)

# # --- File paths --- use remote PC
# biogeo_path <- "/Users/isa128539/Desktop/thesis_data/biogeographical_regions/BiogeoRegions2016.shp"
# wb_drainage_path <- "/Users/isa128539/Desktop/thesis_data/WB_drainage_areas.gpkg"
# wb_coords_path <- "/Users/isa128539/Desktop/thesis_data/bqe_ref_nogeo.parquet"
# model_ready_path <- "/Users/isa128539/Desktop/thesis_data/model_ready.parquet"
# output_path <- "/Users/isa128539/Desktop/thesis_data/model_ready_filtered_biogeo.parquet"
# --- File paths --- use remote mac
biogeo_path <- "~/Documents/ISA_class_materials/Thesis/data/biogeographical_regions/BiogeoRegions2016.shp"
wb_drainage_path <- "~/Documents/ISA_class_materials/Thesis/data/WB_drainage_areas.gpkg"
wb_coords_path <- "~/Documents/ISA_class_materials/Thesis/data/WFD/parquet_tables_joined/bqe_ref_nogeo.parquet"
model_ready_path <- "~/Documents/ISA_class_materials/Thesis/outputs/model/model_ready.parquet"
output_path <- "~/Documents/ISA_class_materials/Thesis/outputs/model/model_ready_biogeo.parquet"

# --- 1. Load biogeographical regions ---
cat("Loading biogeographical regions shapefile...\n")
biogeo <- sf::st_read(biogeo_path)
region_col <- "short_name"
cat("Using region column:", region_col, "\n")
cat("Unique regions:", sort(unique(biogeo[[region_col]])), "\n")

# --- 2. Load pre-dissolved WB drainage areas ---
cat("\nLoading WB_drainage_areas...\n")
wb_drainage <- sf::st_read(wb_drainage_path)
cat("Loaded", nrow(wb_drainage), "water body drainage areas\n")

if (sf::st_crs(wb_drainage) != sf::st_crs(biogeo)) {
  cat("Transforming to match biogeographical regions CRS...\n")
  wb_drainage <- sf::st_transform(wb_drainage, sf::st_crs(biogeo))
}

# --- 3. Pass 1: WB drainage centroids to biogeographical regions ---
cat("\nComputing water body centroids...\n")
wb_centroids <- suppressWarnings(sf::st_centroid(wb_drainage))

cat("Running spatial join...\n")
wb_biogeo <- sf::st_join(wb_centroids, biogeo[, region_col])

wb_region <- sf::st_drop_geometry(wb_biogeo) |>
  dplyr::select(WB_code, biogeographical_region = dplyr::all_of(region_col))

n_assigned <- sum(!is.na(wb_region$biogeographical_region))
cat("Pass 1 assigned:", n_assigned, "/", nrow(wb_region), "\n")

# Nearest-region fallback for centroids outside all regions
unmatched_codes <- wb_region |>
  dplyr::filter(is.na(biogeographical_region)) |>
  dplyr::pull(WB_code)

if (length(unmatched_codes) > 0) {
  cat("Nearest-region fallback for", length(unmatched_codes), "drainage centroids...\n")
  unmatched_centroids <- wb_centroids |>
    dplyr::filter(WB_code %in% unmatched_codes)
  nearest_idx <- sf::st_nearest_feature(unmatched_centroids, biogeo)
  wb_region$biogeographical_region[wb_region$WB_code %in% unmatched_codes] <-
    biogeo[[region_col]][nearest_idx]
}

cat("\nPass 1 region distribution:\n")
print(table(wb_region$biogeographical_region))

# --- 4. Join to model_ready ---
cat("\nLoading model_ready.parquet...\n")
model_ready <- arrow::read_parquet(model_ready_path)
cat("Model ready rows:", nrow(model_ready), "\n")

n_wb_before <- dplyr::n_distinct(model_ready$euSurfaceWaterBodyCode)

model_ready_biogeo <- model_ready |>
  dplyr::left_join(wb_region, by = c("euSurfaceWaterBodyCode" = "WB_code"))

# --- 5. Pass 2: lat/lon fallback for unmatched WBs ---
unmatched_wbs <- model_ready_biogeo |>
  dplyr::filter(is.na(biogeographical_region)) |>
  dplyr::pull(euSurfaceWaterBodyCode) |>
  unique()

if (length(unmatched_wbs) > 0) {
  cat("\nPass 2:", length(unmatched_wbs),
      "WBs not in drainage areas, using lat/lon fallback...\n")
  
  wb_coords <- arrow::read_parquet(wb_coords_path)
  # <-- adjust column names below to match your file
  wb_coords_unmatched <- wb_coords |>
    dplyr::filter(euSurfaceWaterBodyCode %in% unmatched_wbs) |>
    dplyr::distinct(euSurfaceWaterBodyCode, lon, lat)
  
  cat("Found coordinates for", dplyr::n_distinct(wb_coords_unmatched$euSurfaceWaterBodyCode),
      "of", length(unmatched_wbs), "unmatched WBs\n")
  
  # Convert to sf and transform
  wb_pts <- sf::st_as_sf(wb_coords_unmatched,
                         coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(sf::st_crs(biogeo))
  
  # Spatial join
  wb_pts_biogeo <- sf::st_join(wb_pts, biogeo[, region_col])
  
  # Nearest-region fallback for coastal/island points
  still_na <- is.na(wb_pts_biogeo[[region_col]])
  if (any(still_na)) {
    cat("Nearest-region fallback for", sum(still_na), "lat/lon points...\n")
    nearest_idx <- sf::st_nearest_feature(wb_pts_biogeo[still_na, ], biogeo)
    wb_pts_biogeo[[region_col]][still_na] <- biogeo[[region_col]][nearest_idx]
  }
  
  # Patch into model_ready_biogeo
  patch_tbl <- sf::st_drop_geometry(wb_pts_biogeo) |>
    dplyr::select(euSurfaceWaterBodyCode,
                  biogeographical_region = dplyr::all_of(region_col))
  
  model_ready_biogeo <- model_ready_biogeo |>
    dplyr::rows_patch(patch_tbl, by = "euSurfaceWaterBodyCode", unmatched = "ignore")
  
  cat("Pass 2 patched", nrow(patch_tbl), "water bodies\n")
}

# --- 6. Track assignment method ---
model_ready_biogeo <- model_ready_biogeo |>
  dplyr::mutate(biogeo_source = dplyr::if_else(
    euSurfaceWaterBodyCode %in% unmatched_wbs, "wb_point", "drainage_centroid"
  ))

# --- 7. Final validation ---
n_wb_after <- dplyr::n_distinct(model_ready_biogeo$euSurfaceWaterBodyCode)
n_rows_before <- nrow(model_ready)
n_rows_after <- nrow(model_ready_biogeo)
n_still_na <- sum(is.na(model_ready_biogeo$biogeographical_region))

cat("\n===== VALIDATION =====\n")
cat("Rows before:", n_rows_before, "| Rows after:", n_rows_after, "\n")
cat("Water bodies before:", n_wb_before, "| Water bodies after:", n_wb_after, "\n")
cat("Rows with NA biogeographical_region:", n_still_na, "\n")
cat("Assignment method:\n")
print(table(model_ready_biogeo$biogeo_source))

if (n_rows_before != n_rows_after) {
  stop("ROW COUNT CHANGED. The join introduced or removed rows.")
}
if (n_wb_before != n_wb_after) {
  stop("WATER BODY COUNT CHANGED. Some water bodies were lost.")
}
if (n_still_na > 0) {
  na_wbs <- model_ready_biogeo |>
    dplyr::filter(is.na(biogeographical_region)) |>
    dplyr::pull(euSurfaceWaterBodyCode) |>
    unique()
  cat("WARNING:", length(na_wbs), "water bodies still have NA region.\n")
  cat("Codes:", head(na_wbs, 20), "\n")
  warning(length(na_wbs), " water bodies have NA biogeographical region - these lack spatial data and will be dropped during driver filtering.")
} else {
  cat("All water bodies have a biogeographical region assigned.\n")
}

cat("\nFinal region distribution across model rows:\n")
print(table(model_ready_biogeo$biogeographical_region))

# --- 8. Save ---
cat("\nSaving to:", output_path, "\n")
arrow::write_parquet(model_ready_biogeo, output_path)
cat("Done.\n")
