library(sf)
library(dplyr)
library(arrow)

gdb_path <- "~/Documents/ISA_class_materials/Thesis/data/CCM2_stuff.gdb"
gpkg_path <- "~/Documents/ISA_class_materials/Thesis/Spatial_workflow_work/Workflow_WFD/Output/Workflow_outputs.gpkg"
parquet_path <- "~/Documents/ISA_class_materials/Thesis/outputs/model/"

# WB to catchment mapping
sWB <- st_read(gpkg_path, layer = "sWB_ccm_tbl") |>
  st_drop_geometry() |>
  dplyr::select(euSurfaceWaterBodyCode = WB_code, WSO1_ID) |>
  dplyr::distinct()

# Catchment to basin mapping
catch_basin <- st_read(gdb_path, layer = "EU_primary_catchments",
                       query = "SELECT WSO1_ID, WSO_ID FROM EU_primary_catchments") |>
  st_drop_geometry() |>
  dplyr::distinct()

# Basin names
basins <- st_read(gdb_path, layer = "EU_basins",
                  query = "SELECT WSO_ID, NAME FROM EU_basins") |>
  st_drop_geometry() |>
  dplyr::distinct()

# Rebuild lookup using WSO_ID instead of NAME
rbd_lookup <- sWB |>
  dplyr::left_join(catch_basin, by = "WSO1_ID") |>
  dplyr::select(euSurfaceWaterBodyCode, ccm2_basin = WSO_ID) |>
  dplyr::distinct() |>
  dplyr::group_by(euSurfaceWaterBodyCode) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

cat("NA basins:", sum(is.na(rbd_lookup$ccm2_basin)), "\n")
cat("Unique basins:", dplyr::n_distinct(rbd_lookup$ccm2_basin, na.rm = TRUE), "\n")

df_raw <- read_parquet(paste0(parquet_path, "model_ready_biogeo.parquet")) |>
  dplyr::left_join(rbd_lookup, by = "euSurfaceWaterBodyCode")

cat("Unique WBs before:", dplyr::n_distinct(df_raw$euSurfaceWaterBodyCode), "\n")

df <- df_raw |>
  dplyr::filter(!is.na(bio01))

cat("Unique WBs after:", dplyr::n_distinct(df$euSurfaceWaterBodyCode), "\n")
cat("Unique WBs dropped:", dplyr::n_distinct(df_raw$euSurfaceWaterBodyCode) - dplyr::n_distinct(df$euSurfaceWaterBodyCode), "\n")
cat("Rows before:", nrow(df_raw), "\n")
cat("Rows after:", nrow(df), "\n")
cat("Basin NAs:", sum(is.na(df$ccm2_basin)), "\n")


# Write output
arrow::write_parquet(x = df, "/Users/eleshuk/Documents/ISA_class_materials/Thesis/outputs/model/model_ready_biogeo_rb.parquet")

# check <- arrow::read_parquet("/Users/eleshuk/Documents/ISA_class_materials/Thesis/outputs/model/model_ready_biogeo_rb.parquet")
# 
# # Check values per year
# check |>
#   dplyr::group_by(cYear) |>
#   dplyr::summarise(n_distinct_values = dplyr::n_distinct(euSurfaceWaterBodyCode))
# #
# # Count NA rows in a column per year
# check |>
#   dplyr::group_by(cYear) |>
#   dplyr::summarise(n_na = sum(is.na(bio01)))
