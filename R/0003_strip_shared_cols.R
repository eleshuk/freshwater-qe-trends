# Strip shared reference and assessment columns from the SWB tables and write them to parquet_tables_stripped
# Reference columns go to swb_reference and assessment columns go to swb_assessment_reference
# The check_* functions are optional QA on the raw and stripped tables and are not run by the pipeline

# Columns that describe the water body and live in swb_reference after stripping
ref_cols <- c(
  "countryGroup", "countryCode", "countryName",
  "euRBDCode", "rbdName", "euSubUnitCode", "subUnitName",
  "surfaceWaterBodyName", "wiseEvolutionType",
  "cArea", "cLength", "surfaceWaterBodyCategory",
  "naturalAWBHMWB", "reservoir", "surfaceWaterBodyTypeCode",
  "surfaceWaterBodyTransboundary", "NCSWaterBodyType",
  "hasDescriptiveData", "fileUrl",
  "swEcologicalStatusOrPotentialValue", "swChemicalStatusValue"
)

# Assessment columns that live in swb_assessment_reference after stripping
assessment_cols <- c(
  "swEcologicalAssessmentYear", "swEcologicalAssessmentConfidence",
  "swEcologicalStatusOrPotentialExpectedAchievementDate",
  "swChemicalAssessmentYear", "swChemicalAssessmentConfidence",
  "swChemicalMonitoringResults", "swChemicalStatusExpectedAchievementDate",
  "swMixingZones", "swMixingZonesProportion"
)

# Columns that are never part of a key, used by check_grain_all()
measure_cols <- c(
  "qeStatusOrPotentialValue", "qeMonitoringResults", "qeMonitoringPeriod",
  "qeStatusOrPotentialChange", "qeStatusOrPotentialComparability",
  "swPrioritySubstanceCausingFailure", "swPrioritySubstanceExceedanceInMixingZone",
  "uPBT", "protectedAreaObjectivesSet", "protectedAreaObjectivesMet",
  "swEcologicalExemptionPressure", "swEcologicalExemptionPressureGroup",
  "swSignificantImpactType", "swSignificantPressureType", "swSignificantPressureTypeGroup",
  "qeGroup", "qeEcologicalExemptionTypeGroup"
)

# List the SWB tables in a parquet folder, skipping deprecated ones
list_swb_tables <- function(parquet_root) {
  list.files(parquet_root) |>
    stringr::str_subset("^swb_") |>
    stringr::str_subset("deprecated", negate = TRUE)
}

# Strip the shared columns from each table and write the reference and assessment tables
strip_shared_cols <- function(parquet_root, parquet_root_stripped, top_tables = list_swb_tables(parquet_root)) {
  # Stripped from all non-swb tables only, these live in the SWB table
  non_swb_cols <- c("swSignificantPressureOther", "swSignificantImpactOther")

  # Write each stripped table, skipping tables that already exist
  top_tables |>
    purrr::discard(\(x) dir.exists(file.path(parquet_root_stripped, x))) |>
    purrr::walk(\(x) {
      message("Stripping ", x)
      extra <- if (x != "swb_surface_water_body") non_swb_cols else character(0)
      arrow::open_dataset(file.path(parquet_root, x)) |>
        dplyr::select(-dplyr::any_of(c(ref_cols, assessment_cols, extra))) |>
        arrow::write_dataset(file.path(parquet_root_stripped, x))
    })

  # Write the reference table from the main SWB table
  ref_out <- file.path(parquet_root_stripped, "swb_reference")
  if (!dir.exists(ref_out)) {
    arrow::open_dataset(file.path(parquet_root, "swb_surface_water_body")) |>
      dplyr::select(cYear, euSurfaceWaterBodyCode, dplyr::all_of(ref_cols)) |>
      arrow::write_dataset(ref_out)
  } else {
    message("swb_reference already exists, skipping write")
  }

  # Write the assessment table from every table that has assessment columns
  assess_out <- file.path(parquet_root_stripped, "swb_assessment_reference")
  if (!dir.exists(assess_out)) {
    top_tables |>
      purrr::map(\(x) {
        ds <- arrow::open_dataset(file.path(parquet_root, x))
        present <- intersect(assessment_cols, names(ds))
        if (length(present) == 0) return(NULL)
        ds |>
          dplyr::select(euSurfaceWaterBodyCode, cYear, dplyr::all_of(present)) |>
          dplyr::distinct() |>
          dplyr::collect()
      }) |>
      purrr::keep(\(x) !is.null(x)) |>
      purrr::list_rbind() |>
      dplyr::distinct() |>
      arrow::write_dataset(assess_out)
  } else {
    message("swb_assessment_reference already exists, skipping write")
  }

  invisible(parquet_root_stripped)
}

# Check there is one row per water body per cycle in the main SWB table
check_swb_integrity <- function(parquet_root) {
  duplicates <- arrow::open_dataset(file.path(parquet_root, "swb_surface_water_body")) |>
    dplyr::select(euSurfaceWaterBodyCode, cYear) |>
    dplyr::collect() |>
    dplyr::count(euSurfaceWaterBodyCode, cYear) |>
    dplyr::filter(n > 1)

  if (nrow(duplicates) == 0) {
    message("All good, there is one row per water body per cycle in SWB main table")
  } else {
    message(glue::glue("{nrow(duplicates)} duplicate cYear + euSurfaceWaterBodyCode combinations found in swb_surface_water_body"))
  }
  duplicates
}

# Columns present in every one of top_tables
get_shared_cols <- function(parquet_root, top_tables = list_swb_tables(parquet_root)) {
  top_tables |>
    purrr::map(\(tbl) names(arrow::open_dataset(file.path(parquet_root, tbl)))) |>
    purrr::reduce(intersect)
}

# Compare the shared columns in every other SWB table against the main SWB table
check_swb_consistency <- function(parquet_root, top_tables = list_swb_tables(parquet_root),
                                  shared_cols = get_shared_cols(parquet_root, top_tables)) {
  join_keys <- c("cYear", "euSurfaceWaterBodyCode")
  check_cols <- setdiff(shared_cols, join_keys)
  other_tables <- setdiff(top_tables, "swb_surface_water_body")

  # Reference values from the main SWB table
  reference <- arrow::open_dataset(file.path(parquet_root, "swb_surface_water_body")) |>
    dplyr::select(dplyr::all_of(unique(c(join_keys, shared_cols)))) |>
    dplyr::collect()

  # Count mismatches per table and column, treating NA against a value as a mismatch
  purrr::map(other_tables, \(tbl) {
    candidate <- arrow::open_dataset(file.path(parquet_root, tbl)) |>
      dplyr::select(dplyr::all_of(unique(c(join_keys, shared_cols)))) |>
      dplyr::collect() |>
      dplyr::distinct()

    joined <- dplyr::inner_join(reference, candidate, by = join_keys, suffix = c("_ref", "_cand"))

    purrr::map(check_cols, \(col) {
      n_total <- nrow(joined)
      n_mismatch <- sum(
        joined[[paste0(col, "_ref")]] != joined[[paste0(col, "_cand")]] |
          (is.na(joined[[paste0(col, "_ref")]]) != is.na(joined[[paste0(col, "_cand")]])),
        na.rm = TRUE
      )
      tibble::tibble(
        table = tbl,
        column = col,
        n_total = n_total,
        n_mismatch = n_mismatch,
        mismatch_pct = round(100 * n_mismatch / n_total, 2)
      )
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()
}

# Count cYear and water body combinations in each child table that are missing from the main SWB table
check_join_keys <- function(parquet_root, top_tables = list_swb_tables(parquet_root)) {
  join_keys <- c("cYear", "euSurfaceWaterBodyCode")
  other_tables <- setdiff(top_tables, "swb_surface_water_body")

  # Distinct keys in the main SWB table
  reference <- arrow::open_dataset(file.path(parquet_root, "swb_surface_water_body")) |>
    dplyr::select(dplyr::all_of(join_keys)) |>
    dplyr::distinct() |>
    dplyr::collect()

  # Anti join each child table against the reference keys
  purrr::map(other_tables, \(tbl) {
    candidate <- arrow::open_dataset(file.path(parquet_root, tbl)) |>
      dplyr::select(dplyr::all_of(join_keys)) |>
      dplyr::distinct() |>
      dplyr::collect()

    tibble::tibble(
      table = tbl,
      n_missing_from_reference = nrow(dplyr::anti_join(candidate, reference, by = join_keys))
    )
  }) |>
    purrr::list_rbind()
}

# Find water bodies whose assessment columns have more than one distinct value across tables
check_assessment <- function(parquet_root_stripped) {
  table_dirs <- list.dirs(parquet_root_stripped, recursive = FALSE)

  # Read the assessment columns present in one table
  read_assessment <- function(dir) {
    tbl <- arrow::read_parquet(file.path(dir, "part-0.parquet"))
    cols_present <- intersect(assessment_cols, names(tbl))
    if (!"euSurfaceWaterBodyCode" %in% names(tbl)) return(NULL)
    if (length(cols_present) == 0) return(NULL)
    tbl |>
      dplyr::select(euSurfaceWaterBodyCode, cYear, dplyr::all_of(cols_present)) |>
      dplyr::distinct() |>
      dplyr::mutate(source_table = basename(dir))
  }

  # Flag water bodies where any assessment column has more than one non-NA value
  purrr::map(table_dirs, read_assessment) |>
    purrr::keep(\(x) !is.null(x)) |>
    purrr::list_rbind() |>
    dplyr::select(-source_table) |>
    dplyr::group_by(cYear, euSurfaceWaterBodyCode) |>
    dplyr::summarise(
      dplyr::across(dplyr::any_of(assessment_cols), \(x) dplyr::n_distinct(x[!is.na(x)])),
      .groups = "drop"
    ) |>
    dplyr::filter(dplyr::if_any(dplyr::any_of(assessment_cols), \(x) x > 1))
}

# Check whether each table has one row per combination of join_keys
check_grain <- function(parquet_root, top_tables, join_keys) {
  purrr::map(top_tables, \(tbl) {
    ds <- arrow::open_dataset(file.path(parquet_root, tbl, "part-0.parquet"))

    n_rows <- ds |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)

    n_distinct_keys <- ds |>
      dplyr::select(dplyr::all_of(join_keys)) |>
      dplyr::distinct() |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)

    tibble::tibble(
      table = tbl,
      n_rows = n_rows,
      n_distinct_keys = n_distinct_keys,
      is_one_row_per_key = n_rows == n_distinct_keys
    )
  }) |>
    purrr::list_rbind()
}

# Check grain for every table, using all non-measure columns as the key
check_grain_all <- function(parquet_root, exclude_cols = measure_cols) {
  tables <- list.dirs(parquet_root, recursive = FALSE, full.names = FALSE)

  purrr::map(tables, \(tbl) {
    ds <- arrow::open_dataset(file.path(parquet_root, tbl, "part-0.parquet"))
    key_cols <- setdiff(names(ds), exclude_cols)

    n_rows <- ds |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)

    n_distinct_keys <- ds |>
      dplyr::select(dplyr::all_of(key_cols)) |>
      dplyr::distinct() |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)

    tibble::tibble(
      table = tbl,
      n_rows = n_rows,
      n_distinct_keys = n_distinct_keys,
      is_one_row_per_key = n_rows == n_distinct_keys,
      key_cols = list(key_cols)
    )
  }) |>
    purrr::list_rbind()
}
