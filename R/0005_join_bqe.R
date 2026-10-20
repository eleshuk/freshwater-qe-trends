# Create views to be used for analysis
# Joins quality_element table with swb_assessment_reference and filters to desired meterics
  # Kept only quality element status class 1-5, removed TeW
  # Only monitoring or expert judgement only with high confidence
  # United Kingdom (UK) removed
  # Coastal waters (CW) removed
  # Angiosperms and macroalgae removed
  # Added covariates
  # Added biogeographical region
  # River basin added

# Join the quality element table to the reference table and write joined_bqe_ref_w2010_final.parquet
# The defaults give the thesis dataset; drop_qe, drop_categories and drop_countries can be changed
# (e.g. set to NULL) to build less filtered versions, written under file_stem
create_qe_ref_join_filtered <- function(wfd_root, qe_interest,
                                        drop_qe = c("QE1-2-2 - Angiosperms", "QE1-2-1 - Macroalgae"),
                                        drop_categories = c("TeW", "CW"),
                                        drop_countries = "United Kingdom",
                                        file_stem = NULL,
                                        export_csv = FALSE, export_parquet = FALSE) {
  
  parquet_root_cleaned <- file.path(wfd_root, "parquet_tables_cleaned")
  monitoring_type <- c("Expert judgement", "Monitoring")
  
  # Filter out inapplicable, unknown and none, and the dropped qe codes
  # Plain read_parquet is used because arrow's dataset functions can fail once sf/terra are loaded in the session
  all_bqe <- arrow::read_parquet(
    file.path(parquet_root_cleaned, "swb_surface_water_body_quality_element", "part-0.parquet")
  ) |>
    dplyr::filter(!qeStatusOrPotentialValue %in% c("Unknown", "Inapplicable", "None"), !qeCode %in% drop_qe)
  
  # Find water body/QE pairs with desired monitoring type in both 2016 and 2022
  valid_pairs <- all_bqe |>
    dplyr::filter(cYear %in% c(2016, 2022), qeMonitoringResults %in% monitoring_type) |>
    dplyr::distinct(euSurfaceWaterBodyCode, qeCode, cYear) |>
    dplyr::group_by(euSurfaceWaterBodyCode, qeCode) |>
    dplyr::filter(dplyr::n_distinct(cYear) == 2) |>
    dplyr::ungroup() |>
    dplyr::distinct(euSurfaceWaterBodyCode, qeCode)
  
  # 2016 and 2022 data filtered by monitoring type
  bqe_df <- all_bqe |>
    dplyr::filter(
      cYear %in% c(2016, 2022),
      qeGroup %in% qe_interest,
      qeMonitoringResults %in% monitoring_type
    )
  
  # 2010 data selected by valid_pairs
  bqe_2010 <- all_bqe |>
    dplyr::filter(cYear == 2010, qeGroup %in% qe_interest) |>
    dplyr::semi_join(valid_pairs, by = c("euSurfaceWaterBodyCode", "qeCode"))
  
  # For 2010 water bodies not covered by valid_pairs, include those with high assessment confidence
  assessment_table <- arrow::read_parquet(
    file.path(parquet_root_cleaned, "swb_assessment_reference", "part-0.parquet"),
    col_select = c("euSurfaceWaterBodyCode", "cYear", "swEcologicalAssessmentConfidence")
  )
  
  valid_pair_codes <- dplyr::distinct(valid_pairs, euSurfaceWaterBodyCode)
  
  bqe_2010_extra <- all_bqe |>
    dplyr::filter(cYear == 2010, qeGroup %in% qe_interest) |>
    dplyr::anti_join(valid_pair_codes, by = "euSurfaceWaterBodyCode") |>
    dplyr::left_join(
      assessment_table |> dplyr::filter(cYear == 2010),
      by = c("euSurfaceWaterBodyCode", "cYear"),
      relationship = "many-to-one"
    ) |>
    dplyr::filter(swEcologicalAssessmentConfidence == "High") |>
    dplyr::select(-swEcologicalAssessmentConfidence)
  
  bqe_df <- dplyr::bind_rows(bqe_df, bqe_2010, bqe_2010_extra)
  
  cols_of_interest <- c(
    "cYear", "euSurfaceWaterBodyCode", "countryCode", "countryName", "rbdName",
    "swEcologicalStatusOrPotentialValue", "swChemicalStatusValue", "surfaceWaterBodyName",
    "wiseEvolutionType", "surfaceWaterBodyCategory"
  )
  
  ref <- arrow::read_parquet(
    file.path(parquet_root_cleaned, "swb_reference", "part-0.parquet"),
    col_select = dplyr::all_of(cols_of_interest)
  )
  
  bqe_monitoring_map <- c("1" = "High", "2" = "Good", "3" = "Moderate", "4" = "Poor", "5" = "Bad")
  
  joined_qe_df <- bqe_df |>
    dplyr::left_join(ref, by = c("cYear", "euSurfaceWaterBodyCode"), relationship = "many-to-one") |>
    dplyr::arrange(cYear) |>
    dplyr::filter(
      !is.na(qeStatusOrPotentialValue),
      qeStatusOrPotentialValue != "Unknown",
      # Filter out territorial and coastal waters (by default)
      !surfaceWaterBodyCategory %in% drop_categories,
      # Filter out the UK (by default)
      !countryName %in% drop_countries
    ) |>
    dplyr::mutate(
      qeStatusOrPotentialValueLabel = factor(
        bqe_monitoring_map[qeStatusOrPotentialValue],
        levels = bqe_monitoring_map
      )
    )
  
  date_prefix <- format(Sys.Date(), "%Y%m%d")
  out_dir <- file.path(wfd_root, "parquet_tables_joined")
  
  # Use a distinct filename when qe_interest covers more than just QE1, unless a name is given
  if (is.null(file_stem)) {
    file_stem <- if (identical(qe_interest, "QE1 - Biological quality elements")) "joined_bqe_ref_w2010" else "joined_all_qe_ref_w2010"
  }
  
  if (export_csv) {
    readr::write_csv(joined_qe_df, file.path(wfd_root, paste0(date_prefix, "_", file_stem, ".csv")))
  }
  if (export_parquet) {
    arrow::write_parquet(joined_qe_df, file.path(out_dir, paste0(file_stem, "_final.parquet")))
  }
  
  joined_qe_df
}
