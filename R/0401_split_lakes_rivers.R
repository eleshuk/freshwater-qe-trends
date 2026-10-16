# Split the model-ready dataset into one dataframe for lakes and one for rivers

# Write lakes_model_df.parquet and rivers_model_df.parquet to model_dir
split_lakes_rivers <- function(model_dir, input_file = "model_ready_biogeo_rb.parquet") {
  df <- arrow::read_parquet(file.path(model_dir, input_file))

  # Filter each category, print water bodies per cycle and write
  categories <- c(lakes = "LW", rivers = "RW")
  purrr::iwalk(categories, \(category, name) {
    out <- df |>
      dplyr::filter(surfaceWaterBodyCategory == category)

    out |>
      dplyr::summarise(n_water_bodies = dplyr::n_distinct(euSurfaceWaterBodyCode), .by = cYear) |>
      print()

    arrow::write_parquet(out, file.path(model_dir, paste0(name, "_model_df.parquet")))
  })

  invisible(model_dir)
}
