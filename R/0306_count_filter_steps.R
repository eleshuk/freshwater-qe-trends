# Number of unique surface water bodies per reporting cycle after each filtering and integration step (thesis Table 7)
# Each step is applied to the result of the row above; the last three rows are read from the model-ready files

# Unique water bodies per cycle in a table with cYear and euSurfaceWaterBodyCode
count_wb_per_cycle <- function(df, step) {
  df |>
    dplyr::distinct(cYear, euSurfaceWaterBodyCode) |>
    dplyr::count(cYear = as.character(cYear), name = "n") |>
    tidyr::pivot_wider(names_from = cYear, values_from = n) |>
    dplyr::mutate(step = step, .before = 1)
}

# Build the step-by-step counts and write them as a CSV
count_filter_steps <- function(wfd_root, model_dir, out_path) {
  cleaned <- file.path(wfd_root, "parquet_tables_cleaned")
  angio_macro <- c("QE1-2-2 - Angiosperms", "QE1-2-1 - Macroalgae")

  # Biological quality elements joined to the water body category
  qe <- arrow::read_parquet(
    file.path(cleaned, "swb_surface_water_body_quality_element", "part-0.parquet"),
    col_select = c("cYear", "euSurfaceWaterBodyCode", "qeGroup", "qeStatusOrPotentialValue")
  ) |>
    dplyr::filter(qeGroup == "QE1 - Biological quality elements")
  ref <- arrow::read_parquet(
    file.path(cleaned, "swb_reference", "part-0.parquet"),
    col_select = c("cYear", "euSurfaceWaterBodyCode", "surfaceWaterBodyCategory")
  )
  status_1_5 <- qe |>
    dplyr::filter(qeStatusOrPotentialValue %in% as.character(1:5)) |>
    dplyr::left_join(ref, by = c("cYear", "euSurfaceWaterBodyCode"), relationship = "many-to-one") |>
    dplyr::filter(surfaceWaterBodyCategory != "TeW")

  # Monitoring filter with no other exclusions, then each exclusion in turn
  monitored <- create_qe_ref_join_filtered(
    wfd_root, "QE1 - Biological quality elements",
    drop_qe = NULL, drop_categories = "TeW", drop_countries = NULL
  )
  no_uk <- dplyr::filter(monitored, countryName != "United Kingdom")
  no_cw <- dplyr::filter(no_uk, surfaceWaterBodyCategory != "CW")
  no_angio <- dplyr::filter(no_cw, !qeCode %in% angio_macro)

  # Integration steps read from the model-ready files
  model_cols <- c("cYear", "euSurfaceWaterBodyCode")
  read_model <- \(f) arrow::read_parquet(file.path(model_dir, f), col_select = dplyr::all_of(model_cols))

  counts <- dplyr::bind_rows(
    count_wb_per_cycle(qe, "No filter"),
    count_wb_per_cycle(status_1_5, "Kept only quality element status class 1-5, removed TeW"),
    count_wb_per_cycle(monitored, "Only monitoring or expert judgement only with high confidence"),
    count_wb_per_cycle(no_uk, "United Kingdom (UK) removed"),
    count_wb_per_cycle(no_cw, "Coastal waters (CW) removed"),
    count_wb_per_cycle(no_angio, "Angiosperms and macroalgae removed"),
    count_wb_per_cycle(read_model("model_ready.parquet"), "Added covariates"),
    count_wb_per_cycle(read_model("model_ready_biogeo.parquet"), "Added biogeographical region"),
    count_wb_per_cycle(read_model("model_ready_biogeo_rb.parquet"), "River basin added")
  )

  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(counts, out_path)
  print(counts)
  invisible(counts)
}
