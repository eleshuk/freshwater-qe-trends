# Checks on the saved pipeline outputs, run after the pipeline and before writing up results
# Every check is reported in one table and the script stops at the end if any failed

library(arrow)
library(dplyr)

# Paths to the pipeline outputs
wfd_root <- "data/WFD"
model_dir <- "data/model"
m4_dir <- file.path(model_dir, "M4", "m4_stage1_results_rivers")

# Thresholds: missing share per covariate and cycle, optimizer gradient, main vs sensitivity coefficients
max_pct_na <- 5
gradient_limit <- 1e-3
max_coef_diff <- 0.05

# Covariates used in the river models
model_vars <- c(
  "bio05", "bio04", "log_bio15", "sqrt_bio18",
  "log_hu_deficit_coldest_qtr", "log_hu_deficit_driest_qtr",
  "p_surplus_w", "log_imperv_pct", "prop_agricultural", "log_prop_wetlands",
  "amber_n_total_log",
  "pressure_diffuse_sources", "pressure_point_sources",
  "pressure_hydromorphology", "pressure_abstraction",
  "pressure_introduced_species_litter", "pressure_anthropogenic_other",
  "pressure_anthropogenic_unknown"
)

# One result row per check
check <- function(name, ok, detail = "") {
  tibble::tibble(check = name, passed = isTRUE(ok), detail = paste(detail, collapse = "; "))
}

# Number of key combinations that appear more than once
n_dup_keys <- function(df, keys) {
  df |>
    dplyr::count(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::filter(n > 1) |>
    nrow()
}

# Rows that are NaN or infinite in any of the given columns
n_non_finite <- function(df, cols) {
  df |>
    dplyr::filter(dplyr::if_any(dplyr::all_of(cols), \(x) is.nan(x) | is.infinite(x))) |>
    nrow()
}

results <- list()
cyear_class <- list()

# Cleaned WFD tables: leftover "Unpopulated" sentinel values by table and column
cleaned_root <- file.path(wfd_root, "parquet_tables_cleaned")
sentinel_hits <- list.dirs(cleaned_root, recursive = FALSE) |>
  purrr::map(\(dir) {
    df <- arrow::open_dataset(dir) |> dplyr::collect()
    chr_cols <- names(df)[purrr::map_lgl(df, is.character)]
    tibble::tibble(
      table = basename(dir),
      column = chr_cols,
      n = purrr::map_int(chr_cols, \(col) sum(tolower(df[[col]]) == "unpopulated", na.rm = TRUE))
    )
  }) |>
  purrr::list_rbind() |>
  dplyr::filter(n > 0)

results$cleaned <- check(
  "No 'Unpopulated' values left in cleaned tables",
  nrow(sentinel_hits) == 0,
  paste0(sentinel_hits$table, ".", sentinel_hits$column, " (", sentinel_hits$n, ")")
)

# Filtered reference table against the BQE table
ref_filtered <- arrow::read_parquet(file.path(cleaned_root, "swb_reference_filtered", "part-0.parquet"))
bqe <- arrow::read_parquet(file.path(wfd_root, "parquet_tables_joined", "joined_bqe_ref_w2010_final.parquet"))
cyear_class$ref_filtered <- class(ref_filtered$cYear)[1]
cyear_class$bqe <- class(bqe$cYear)[1]

wb_counts <- dplyr::full_join(
  ref_filtered |> dplyr::distinct(cYear, euSurfaceWaterBodyCode) |> dplyr::count(cYear, name = "n_ref"),
  bqe |> dplyr::distinct(cYear, euSurfaceWaterBodyCode) |> dplyr::count(cYear, name = "n_bqe"),
  by = "cYear"
)
n_bqe_rows <- nrow(bqe)

# Status values that the High-Bad label map cannot translate
bad_status <- setdiff(unique(bqe$qeStatusOrPotentialValue), as.character(1:5))

results$bqe <- dplyr::bind_rows(
  check(
    "Reference and BQE tables have the same water bodies per cycle",
    all(wb_counts$n_ref == wb_counts$n_bqe),
    paste0(wb_counts$cYear, ": ", wb_counts$n_ref, " vs ", wb_counts$n_bqe)
  ),
  check(
    "Reference table has one row per water body and cycle",
    n_dup_keys(ref_filtered, c("cYear", "euSurfaceWaterBodyCode")) == 0,
    n_dup_keys(ref_filtered, c("cYear", "euSurfaceWaterBodyCode"))
  ),
  check(
    "BQE table has one row per water body, cycle and BQE",
    n_dup_keys(bqe, c("cYear", "euSurfaceWaterBodyCode", "qeCode")) == 0,
    n_dup_keys(bqe, c("cYear", "euSurfaceWaterBodyCode", "qeCode"))
  ),
  check("BQE status values are all 1 to 5", length(bad_status) == 0, bad_status),
  check(
    "Only the 2010, 2016 and 2022 cycles",
    all(as.character(bqe$cYear) %in% c("2010", "2016", "2022")),
    sort(unique(as.character(bqe$cYear)))
  ),
  check(
    "No coastal, transitional or UK water bodies",
    !any(bqe$surfaceWaterBodyCategory %in% c("TeW", "CW") | bqe$countryName == "United Kingdom", na.rm = TRUE)
  ),
  check("Every status has a High-Bad label", !anyNA(bqe$qeStatusOrPotentialValueLabel))
)
rm(ref_filtered, bqe)

# Pressure matrix with drivers
pm <- arrow::read_parquet(file.path(model_dir, "pressure_matrix_with_drivers.parquet"))
cyear_class$pm <- class(pm$cYear)[1]
pressure_cols <- grep("^pressure_", names(pm), value = TRUE)
failing_cols <- grep("^failing_", names(pm), value = TRUE)
clc_cols <- grep("^clc_.*_m2$", names(pm), value = TRUE)
hu_cols <- intersect(c("hu_annual_mean", "hu_max_month", "hu_min_month", "hu_driest_qtr", "hu_warmest_qtr", "hu_coldest_qtr", "hu_wettest_qtr"), names(pm))

# Annual precipitation must be at least the wettest quarter, which must be at least the wettest month
n_bio_order <- pm |>
  dplyr::filter(bio12 < bio16 | bio16 < bio13) |>
  nrow()

# Water bodies whose land cover areas are all zero, which would give undefined shares
n_zero_clc <- pm |>
  dplyr::filter(rowSums(dplyr::pick(dplyr::all_of(clc_cols))) == 0) |>
  nrow()

results$pm <- dplyr::bind_rows(
  check(
    "Pressure matrix has one row per water body and cycle",
    n_dup_keys(pm, c("cYear", "euSurfaceWaterBodyCode")) == 0,
    n_dup_keys(pm, c("cYear", "euSurfaceWaterBodyCode"))
  ),
  check("Pressure flags are 0 or 1", all(unlist(pm[pressure_cols]) %in% c(0L, 1L))),
  check("Substance flags are 0, 1 or NA", all(unlist(pm[failing_cols]) %in% c(0L, 1L, NA))),
  check("Humidity values sit between 0 and 100", all(dplyr::between(unlist(pm[hu_cols]), 0, 100), na.rm = TRUE)),
  check("bio12 >= bio16 >= bio13", n_bio_order == 0, paste(n_bio_order, "rows (set to NA by the transform)")),
  check("No water bodies with zero total land cover area", n_zero_clc == 0, paste(n_zero_clc, "rows"))
)
rm(pm)

# Model-ready table and its biogeography and basin version
model_ready <- arrow::read_parquet(file.path(model_dir, "model_ready.parquet"))
model_rb <- arrow::read_parquet(file.path(model_dir, "model_ready_biogeo_rb.parquet"))
n_with_drivers <- sum(!is.na(model_ready$bio01))
keys <- c("cYear", "euSurfaceWaterBodyCode", "qeCode")

results$model_ready <- dplyr::bind_rows(
  check(
    "Model-ready table keeps one row per BQE record",
    nrow(model_ready) == n_bqe_rows,
    paste(nrow(model_ready), "vs", n_bqe_rows)
  ),
  check("Model-ready table has unique keys", n_dup_keys(model_ready, keys) == 0, n_dup_keys(model_ready, keys)),
  check(
    "Biogeography and basin joins neither add nor drop rows",
    nrow(model_rb) == n_with_drivers,
    paste(nrow(model_rb), "vs", n_with_drivers, "rows with drivers")
  ),
  check("Biogeography and basin table has unique keys", n_dup_keys(model_rb, keys) == 0, n_dup_keys(model_rb, keys)),
  check("Every row has a basin", !anyNA(model_rb$ccm2_basin), paste(sum(is.na(model_rb$ccm2_basin)), "missing")),
  check(
    "Every row has a biogeographical region",
    !anyNA(model_rb$biogeographical_region),
    paste(sum(is.na(model_rb$biogeographical_region)), "missing")
  )
)
n_rivers_expected <- sum(model_rb$surfaceWaterBodyCategory == "RW" & !model_rb$countryCode %in% c("CY", "MT"))
rm(model_ready, model_rb)

# Transformed river data used by the models
rivers <- arrow::read_parquet(file.path(model_dir, "rivers_model_df_transformed.parquet"))
params <- readr::read_csv(file.path(model_dir, "skew_check", "rivers_transform_parameters.csv"), show_col_types = FALSE) |>
  tibble::deframe()
prop_cols <- c("prop_artificial", "prop_agricultural", "prop_forest", "prop_wetlands")
tol <- 1e-9

# Covariates whose missing share in a cycle exceeds the threshold
na_by_cycle <- rivers |>
  dplyr::summarise(dplyr::across(dplyr::all_of(model_vars), \(x) 100 * mean(is.na(x))), .by = cYear) |>
  tidyr::pivot_longer(-cYear, names_to = "variable", values_to = "pct_na") |>
  dplyr::filter(pct_na > max_pct_na)

results$rivers <- dplyr::bind_rows(
  check(
    "River table has the expected rows",
    nrow(rivers) == n_rivers_expected,
    paste(nrow(rivers), "vs", n_rivers_expected)
  ),
  check("Only river water bodies, without Cyprus or Malta", all(rivers$surfaceWaterBodyCategory == "RW") && !any(rivers$countryCode %in% c("CY", "MT"))),
  check("No NaN or infinite model covariates", n_non_finite(rivers, model_vars) == 0, paste(n_non_finite(rivers, model_vars), "rows")),
  check("Land cover shares sit between 0 and 1", all(dplyr::between(unlist(rivers[prop_cols]), 0, 1), na.rm = TRUE)),
  check(
    "Nitrogen and phosphorus stay inside the winsorising limits",
    all(dplyr::between(rivers$n_surplus_w, params[["n_lower"]] - tol, params[["n_upper"]] + tol), na.rm = TRUE) &&
      all(dplyr::between(rivers$p_surplus_w, params[["p_lower"]] - tol, params[["p_upper"]] + tol), na.rm = TRUE)
  ),
  check(
    paste0("No covariate is more than ", max_pct_na, "% missing in any cycle"),
    nrow(na_by_cycle) == 0,
    paste0(na_by_cycle$variable, " ", na_by_cycle$cYear, " (", round(na_by_cycle$pct_na, 1), "%)")
  )
)
rm(rivers)

# Cycle stored with the same type in every table before modelling
results$types <- check(
  "cYear has the same type in the reference, BQE and pressure tables",
  length(unique(unlist(cyear_class))) == 1,
  paste(names(cyear_class), unlist(cyear_class), sep = ": ")
)

# Saved model fits: nlminb convergence code and largest gradient
fit_files <- list.files(file.path(m4_dir, "RW"), pattern = "^fit_.*\\.RData$", full.names = TRUE)
fit_summary <- purrr::map(fit_files, \(path) {
  env <- new.env()
  load(path, envir = env)
  fit <- env$bqe_result$fit
  grad <- if (is.numeric(fit$gradient)) fit$gradient else fit$optRes$gradient
  tibble::tibble(
    bqe = sub("^fit_(.*)\\.RData$", "\\1", basename(path)),
    convergence = fit$optRes$convergence,
    max_gradient = if (is.numeric(grad)) max(abs(grad)) else NA_real_
  )
}) |>
  purrr::list_rbind()

not_converged <- dplyr::filter(fit_summary, convergence != 0)
high_gradient <- dplyr::filter(fit_summary, max_gradient > gradient_limit)

results$fits <- dplyr::bind_rows(
  check("Model fits found", nrow(fit_summary) > 0, paste(nrow(fit_summary), "fits")),
  check(
    "Every fit has nlminb convergence code 0",
    nrow(not_converged) == 0,
    paste0(not_converged$bqe, " (code ", not_converged$convergence, ")")
  ),
  check(
    paste("Largest absolute gradient below", gradient_limit),
    nrow(high_gradient) == 0,
    paste0(high_gradient$bqe, " (", signif(high_gradient$max_gradient, 3), ")")
  )
)

# Main against sensitivity-run coefficients, for BQEs fitted in both
sens_dir <- file.path(m4_dir, "RW_sens")
if (dir.exists(sens_dir)) {
  shared_files <- intersect(
    list.files(file.path(m4_dir, "RW"), pattern = "^coefficients_.*\\.csv$"),
    list.files(sens_dir, pattern = "^coefficients_.*\\.csv$")
  )
  coef_diff <- purrr::map(shared_files, \(f) {
    dplyr::inner_join(
      readr::read_csv(file.path(m4_dir, "RW", f), show_col_types = FALSE),
      readr::read_csv(file.path(sens_dir, f), show_col_types = FALSE),
      by = "variable",
      suffix = c("_main", "_sens")
    ) |>
      dplyr::summarise(max_diff = max(abs(estimate_main - estimate_sens))) |>
      dplyr::mutate(bqe = sub("^coefficients_(.*)\\.csv$", "\\1", f))
  }) |>
    purrr::list_rbind() |>
    dplyr::filter(max_diff > max_coef_diff)

  results$sens <- check(
    paste("Main and sensitivity coefficients differ by at most", max_coef_diff),
    nrow(coef_diff) == 0,
    paste0(coef_diff$bqe, " (", signif(coef_diff$max_diff, 3), ")")
  )
}

# Report every check and stop if any failed
report <- dplyr::bind_rows(results)
print(report, n = Inf, width = Inf)
if (any(!report$passed)) {
  stop(sum(!report$passed), " check(s) failed, see the table above")
}
message("All ", nrow(report), " checks passed")
