# Cleaning pipeline for stripped data (stripping happens in 20260504_check_cols_and_grain.R)

library(arrow)
library(dplyr)
library(purrr)
library(stringr)

parquet_root_stripped <- "/Users/eleshuk/Documents/ISA_class_materials/Thesis/data/WFD/parquet_tables_stripped"
parquet_root_cleaned <- "/Users/eleshuk/Documents/ISA_class_materials/Thesis/data/WFD/parquet_tables_cleaned"

# Replace sentinel strings with NA across specified columns
fix_values <- function(df, cols, sentinel = c("Unpopulated", "unpopulated")) {
  dplyr::mutate(df, dplyr::across(
    dplyr::all_of(cols),
    \(x) dplyr::if_else(x %in% sentinel, NA, x)
  ))
}

# Count unique values per column for spot-checking after cleaning
count_col_values <- function(df, col) {
  df |>
    dplyr::count(.data[[col]], sort = TRUE)
}

# Load, transform, write. Skips if already exists
clean_and_write <- function(table_name, transform_fn) {
  out_path <- file.path(parquet_root_cleaned, table_name)
  if (dir.exists(out_path)) {
    message(table_name, " already exists, skipping")
    return(invisible(NULL))
  }
  df <- arrow::open_dataset(
    file.path(parquet_root_stripped, table_name, "part-0.parquet")
  ) |>
    dplyr::collect() |>
    transform_fn()
  arrow::write_dataset(df, out_path)
  message(table_name, " written")
}

# Shared column sets
qe_cols <- c(
  "qeStatusOrPotentialValue", "qeMonitoringResults", "qeMonitoringPeriod",
  "qeStatusOrPotentialChange", "qeStatusOrPotentialComparability"
)
priority_substance_cols <- c(
  "swPrioritySubstanceCausingFailure", "swPrioritySubstanceExceedanceInMixingZone", "uPBT"
)
protected_area_cols <- c("protectedAreaObjectivesSet", "protectedAreaObjectivesMet")

# Reference table cleaning
clean_and_write("swb_reference", \(df) {
  dplyr::mutate(df,
                cLength = round(as.numeric(cLength), 3),
                cArea = round(as.numeric(cArea), 3),
                # Convert hasDescriptiveData to logical (T/F)
                hasDescriptiveData = as.logical(as.integer(hasDescriptiveData)),
                # When reservoir is No or Inapplicable, make it "No" - i.e. the water body is not a reservoir
                reservoir = dplyr::case_when(
                  reservoir %in% c("No", "Inapplicable") ~ "No",
                  tolower(reservoir) == "unpopulated" ~ NA,
                  .default = reservoir
                ),
                surfaceWaterBodyTransboundary = dplyr::if_else(
                  tolower(surfaceWaterBodyTransboundary) == "unpopulated", NA, surfaceWaterBodyTransboundary
                )
  )
})

# Assessment table cleaning
clean_and_write("swb_assessment_reference", \(df) {
  fix_values(df, c(
    "swEcologicalAssessmentYear", "swEcologicalStatusOrPotentialExpectedAchievementDate",
    "swChemicalAssessmentConfidence", "swChemicalAssessmentYear",
    "swChemicalMonitoringResults", "swMixingZones", "swChemicalStatusExpectedAchievementDate"
  )) |>
    dplyr::mutate(swMixingZonesProportion = round(as.numeric(swMixingZonesProportion), 3))
})

# Deduplicate upstream artifact before fixing sentinel values - see 20260530_data_quality_investigation
clean_and_write("swb_surface_water_body_quality_element_qe_ecological_exemption_type", \(df) {
  df |>
    dplyr::distinct() |>
    fix_values(qe_cols)
})

# Drop <1% duplicate rows — upstream reporting artifact
clean_and_write("swb_surface_water_body_hmwb_water_use", \(df) {
  dplyr::distinct(df) |>
    fix_values(c("hmwbWaterUse"))
})

# Drop 6 duplicate rows — upstream German reporting artifact (2016 cycle)
clean_and_write("swb_surface_water_body_sw_ecological_exemption_type", \(df) {
  dplyr::distinct(df) |>
    fix_values(c("swEcologicalExemptionPressure", "swEcologicalExemptionPressureGroup"))
})

# Tables that only need Unpopulated/unpopulated fixing
list(
  list("swb_surface_water_body_hmwb_physical_alteration", c("hmwbPhysicalAlteration")),
  # list("swb_surface_water_body_hmwb_water_use", c("hmwbWaterUse")),
  # Same changes as swb_surface_water_body_quality_element
  # Remove bc it has duplicates that are now dealt with above
  # list("swb_surface_water_body_quality_element_qe_ecological_exemption_type", qe_cols),
  # No changes to qeGrouping
  list("swb_surface_water_body_quality_element_qe_grouping", qe_cols),
  list("swb_surface_water_body_sw_associated_protected_area", protected_area_cols),
  # New col - protectedAreaExemption - no changes
  list("swb_surface_water_body_sw_associated_protected_area_protected_area_exemption", protected_area_cols),
  # list("swb_surface_water_body_sw_ecological_exemption_type", c("swEcologicalExemptionPressure", "swEcologicalExemptionPressureGroup")),
  list("swb_surface_water_body_sw_priority_substance", priority_substance_cols),
  # New cols - swChemicalExemptionType, swChemicalExemptionPressure, swChemicalExemptionTypeGroup, swChemicalExemptionPressureGroup - no changes
  list("swb_surface_water_body_sw_priority_substance_sw_chemical_exemption_type", priority_substance_cols),
  # New col - swPrioritySubstanceExceedanceType - no change
  list("swb_surface_water_body_sw_priority_substance_sw_priority_substance_exceedance_type", priority_substance_cols),
  # swSignificantImpactType has Unpopulated
  list("swb_surface_water_body_sw_significant_impact_type", c("swSignificantImpactType")),
  # swSignificantPressureType has Unpopulated
  list("swb_surface_water_body_sw_significant_pressure_type", c("swSignificantPressureType", "swSignificantPressureTypeGroup"))
) |>
  purrr::walk(\(x) clean_and_write(x[[1]], \(df) fix_values(df, x[[2]])))

# quality_element has an additional qeGroup column derived from qeCode
clean_and_write("swb_surface_water_body_quality_element", \(df) {
  fix_values(df, qe_cols) |>
    dplyr::mutate(qeGroup = dplyr::case_when(
      stringr::str_starts(qeCode, "QE1") ~ "QE1 - Biological quality elements",
      stringr::str_starts(qeCode, "QE2") ~ "QE2 - Hydromorphological quality elements",
      stringr::str_starts(qeCode, "QE3") ~ "QE3 - Chemical and physico-chemical quality elements",
      .default = NA_character_
    ))
})

clean_and_write("swb_surface_water_body_failing_rbsp", \(df) {
  # Strip "Other parameter" rows — these exist in swb_surface_water_body_failing_rbsp_sw_failing_rbsp_other
  dplyr::filter(df, swFailingRBSP != "EEA_00-00-0 - Other parameter")
})

# Check which files from the stripped folder didn't need cleaning and copy to the cleaned folder
stripped_tables <- list.dirs(parquet_root_stripped, recursive = FALSE, full.names = FALSE)
cleaned_tables <- list.dirs(parquet_root_cleaned, recursive = FALSE, full.names = FALSE)

# Get tables to copy over, discard swb_surface_water_body_quality_element_group
tables_to_discard <- c("swb_surface_water_body_quality_element_group", "swb_surface_water_body_sw_significant_impact_other", "swb_surface_water_body_sw_significant_pressure_other")

tables_to_copy <- setdiff(stripped_tables, cleaned_tables) |>
  purrr::discard(\(x) x %in% tables_to_discard)

# Copy data that didn't need cleaning to the cleaned folder
purrr::walk(tables_to_copy, \(table) {
  out_path <- file.path(parquet_root_cleaned, table)
  if (dir.exists(out_path)) {
    message(table, " already exists, skipping")
    return(invisible(NULL))
  }
  file.copy(
    from = file.path(parquet_root_stripped, table),
    to = parquet_root_cleaned,
    recursive = TRUE
  )
  message(table, " copied")
})


# Create cleaned 2010-2022 data ref file with swb IDs from the BQE filtered df
# Need to use cYear and SWB code to make sure that we get all of the combos









