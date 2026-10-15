# Code to create 2 model ready dfs
# 1 for lakes, 1 for rivers

library(dplyr)
library(arrow)

# Df with basins and biogeography
df <- arrow::read_parquet("~/Documents/ISA_class_materials/Thesis/outputs/model/model_ready_biogeo_rb.parquet")

# Split out into lakes and rivers
lakes_df <- df |> 
  dplyr::filter(surfaceWaterBodyCategory == "LW")

# Count distinct water bodies per year
lakes_df |> 
  dplyr::group_by(cYear) |> 
  dplyr::summarise(n_lakes = dplyr::n_distinct(euSurfaceWaterBodyCode))

# Write to parquet
arrow::write_parquet(lakes_df, "~/Documents/ISA_class_materials/Thesis/outputs/model/lakes_model_df.parquet")

# Split out into lakes and rivers
rivers_df <- df |> 
  dplyr::filter(surfaceWaterBodyCategory == "RW")

# Count distinct water bodies per year
rivers_df |> 
  dplyr::group_by(cYear) |> 
  dplyr::summarise(n_lakes = dplyr::n_distinct(euSurfaceWaterBodyCode))

# Write to parquet
arrow::write_parquet(rivers_df, "~/Documents/ISA_class_materials/Thesis/outputs/model/rivers_model_df.parquet")
