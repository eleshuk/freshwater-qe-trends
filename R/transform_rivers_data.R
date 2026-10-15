# Paths
model_dir <- "~/Documents/ISA_class_materials/Thesis/outputs/model"
in_path <- file.path(model_dir, "rivers_model_df.parquet")
out_path <- file.path(model_dir, "rivers_model_df_transformed.parquet")
params_path <- file.path(model_dir, "skew_check", "rivers_transform_parameters.csv")

# Helper functions: log constant (1st percentile of non-zero values) and capping
p1_nonzero <- function(x) {
  stats::quantile(x[x > 0 & !is.na(x)], 0.01, names = FALSE)
}

winsorise <- function(x, lim) {
  pmin(pmax(x, lim[1]), lim[2])
}

clc_cols <- c(
  "clc_artificial_m2", "clc_agricultural_m2", "clc_forest_seminatural_m2",
  "clc_wetlands_m2", "clc_water_m2", "clc_other_m2"
)

# Drop Cyprus and Malta (session 4), land cover shares, and removal of impossible bio12 values
rivers <- arrow::read_parquet(in_path) |>
  dplyr::filter(!countryCode %in% c("CY", "MT")) |>
  dplyr::mutate(
    catchment_m2 = rowSums(dplyr::pick(dplyr::all_of(clc_cols))),
    prop_artificial = clc_artificial_m2 / catchment_m2,
    prop_agricultural = clc_agricultural_m2 / catchment_m2,
    prop_forest = clc_forest_seminatural_m2 / catchment_m2,
    prop_wetlands = clc_wetlands_m2 / catchment_m2,
    bio12 = dplyr::if_else(bio12 == 0 | bio12 < bio16 | bio12 < bio13, NA_real_, bio12)
  )

# Covariates are identical across BQEs, so constants come from one row per water body and cycle
wb <- rivers |>
  dplyr::distinct(euSurfaceWaterBodyCode, cYear, .keep_all = TRUE)

params <- list(
  c_bio14 = p1_nonzero(wb$bio14),
  c_imperv = p1_nonzero(wb$mean_imperviousness_pct),
  c_artificial = p1_nonzero(wb$prop_artificial),
  c_wetlands = p1_nonzero(wb$prop_wetlands),
  n_lim = stats::quantile(wb$n_surplus_kg_ha, c(0.01, 0.99), na.rm = TRUE, names = FALSE),
  p_lim = stats::quantile(wb$p_surplus_kg_ha, c(0.01, 0.99), na.rm = TRUE, names = FALSE)
)

# Apply the transforms to every row
rivers <- rivers |>
  dplyr::mutate(
    log_bio12 = log(bio12),
    log_bio13 = log(bio13),
    log_bio14 = log(bio14 + params$c_bio14),
    log_bio15 = log(bio15),
    log_bio16 = log(bio16),
    sqrt_bio17 = sqrt(bio17),
    sqrt_bio18 = sqrt(bio18),
    log_bio19 = log(bio19),
    log_hu_seasonality = log(hu_seasonality),
    log_hu_range = log(hu_range),
    log_hu_deficit_max_month = log(100 - hu_max_month),
    log_hu_deficit_min_month = log(100 - hu_min_month),
    log_hu_deficit_driest_qtr = log(100 - hu_driest_qtr),
    log_hu_deficit_warmest_qtr = log(100 - hu_warmest_qtr),
    log_hu_deficit_coldest_qtr = log(100 - hu_coldest_qtr),
    n_surplus_w = winsorise(n_surplus_kg_ha, params$n_lim),
    p_surplus_w = winsorise(p_surplus_kg_ha, params$p_lim),
    log_imperv_pct = log(mean_imperviousness_pct + params$c_imperv),
    log_prop_artificial = log(prop_artificial + params$c_artificial),
    log_prop_wetlands = log(prop_wetlands + params$c_wetlands),
    amber_n_total_log = log1p(amber_n_total)
  )

# Candidate covariates for the correlation and VIF step (transformed forms)
candidate_vars <- c(
  "bio01", "bio02", "bio03", "bio04", "bio05", "bio06", "bio07", "bio08",
  "bio09", "bio10", "bio11",
  "log_bio12", "log_bio13", "log_bio14", "log_bio15", "log_bio16", "sqrt_bio17",
  "sqrt_bio18", "log_bio19",
  "hu_annual_mean", "log_hu_seasonality", "log_hu_range", "hu_wettest_qtr",
  "log_hu_deficit_max_month", "log_hu_deficit_min_month",
  "log_hu_deficit_driest_qtr", "log_hu_deficit_warmest_qtr",
  "log_hu_deficit_coldest_qtr",
  "n_surplus_w", "p_surplus_w",
  "log_imperv_pct", "log_prop_artificial", "prop_agricultural", "prop_forest",
  "log_prop_wetlands", "amber_n_total_log"
)

# Quick check: no infinite values created by the transforms
inf_counts <- rivers |>
  dplyr::summarise(dplyr::across(dplyr::all_of(candidate_vars), \(x) sum(is.infinite(x))))
stopifnot(all(unlist(inf_counts) == 0))

# Save the data and the constants used, for the methods and for back-transforming
arrow::write_parquet(rivers, out_path)
tibble::tibble(
  parameter = c("c_bio14", "c_imperv", "c_artificial", "c_wetlands",
                "n_lower", "n_upper", "p_lower", "p_upper"),
  value = c(params$c_bio14, params$c_imperv, params$c_artificial,
            params$c_wetlands, params$n_lim, params$p_lim)
) |>
  readr::write_csv(params_path)