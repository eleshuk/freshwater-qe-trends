# Marginal variance partitioning for the river CLMMs (Schulz et al., 2025, eq. 13): thesis Fig. 11 and Table S9
# Each group's share = Cov(group's linear predictor, full fixed linear predictor) / total latent variance,
# so covariance between correlated groups is split between them and the shares sum to 100%
# Reference: Schulz, Saastamoinen & Vanhatalo (2025) Ecological Monographs 95: e1646, doi:10.1002/ecm.1646
# Needs R/0600_model_helpers.R sourced and ggplot2 loaded

# Covariate groups
vp_continuous_vars <- c(
  "bio05", "bio04", "log_bio15", "sqrt_bio18",
  "log_hu_deficit_coldest_qtr", "log_hu_deficit_driest_qtr",
  "p_surplus_w", "log_imperv_pct", "prop_agricultural", "log_prop_wetlands", "amber_n_total_log"
)
vp_pressure_vars <- c(
  "pressure_hydromorphology", "pressure_point_sources", "pressure_diffuse_sources",
  "pressure_abstraction", "pressure_anthropogenic_other", "pressure_introduced_species_litter"
)
vp_reporting_vars <- "pressure_anthropogenic_unknown"

# BQE display labels
vp_bqe_labels <- c(
  "QE1-4 - Fish" = "Fish",
  "QE1-2 - Other aquatic flora" = "Other aquatic flora",
  "QE1-2-3 - Macrophytes" = "Macrophytes",
  "QE1-2-4 - Phytobenthos" = "Phytobenthos",
  "QE1-3 - Benthic invertebrates" = "Benthic invertebrates"
)

# Component order and colours for the plot
component_colours <- c(
  "Continuous drivers" = "#2166AC",
  "Binary pressures" = "#B2182B",
  "Reporting adjustment" = "#8C510A",
  "Cycle" = "#F4A582",
  "Region" = "#FDDBC7",
  "Cycle x Region" = "#D6604D",
  "Water body RE" = "#4DAF4A",
  "Basin RE" = "#984EA3",
  "Country RE" = "#FF7F00",
  "Residual" = "#BDBDBD"
)

# Classify a coefficient name into a group
assign_group <- function(nm) {
  if (nm %in% vp_continuous_vars) return("Continuous drivers")
  if (nm %in% vp_pressure_vars) return("Binary pressures")
  if (nm %in% vp_reporting_vars) return("Reporting adjustment")
  if (grepl("^cycle", nm) && grepl("region", nm)) return("Cycle x Region")
  if (grepl("^cycle", nm)) return("Cycle")
  if (grepl("^region", nm)) return("Region")
  "Other"
}

# Marginal shares of the latent variance for one fitted CLMM, plus the pairwise covariances between groups
partition_marginal <- function(fit, bqe) {
  beta <- fit$beta
  mm <- stats::model.matrix(fit)
  if (is.list(mm)) mm <- mm$X
  mm <- mm[, names(beta), drop = FALSE]
  groups <- vapply(names(beta), assign_group, character(1))

  # Each group's contribution to the linear predictor, one column per group
  eta_groups <- split(seq_along(beta), groups) |>
    purrr::map(\(idx) as.numeric(mm[, idx, drop = FALSE] %*% beta[idx])) |>
    as.data.frame(check.names = FALSE)
  cov_mat <- stats::cov(eta_groups)

  # Random effects and residual are independent of the fixed part by assumption
  re <- ordinal::VarCorr(fit)
  re_var <- \(nm) if (nm %in% names(re)) re[[nm]][1] else 0
  other <- c(
    "Water body RE" = re_var("waterBody"),
    "Basin RE" = re_var("basin"),
    "Country RE" = re_var("country"),
    "Residual" = 1
  )

  # Shares of the true total variance of the latent score
  total <- stats::var(rowSums(eta_groups)) + sum(other)
  bqe_label <- unname(dplyr::coalesce(vp_bqe_labels[bqe], bqe))
  shares <- tibble::tibble(
    bqe = bqe,
    bqe_label = bqe_label,
    component = c(colnames(cov_mat), names(other)),
    pct_marginal = 100 * c(rowSums(cov_mat), other) / total
  ) |>
    dplyr::filter(!(component == "Other" & abs(pct_marginal) < 1e-8))

  # Pairwise covariances between groups, as % of the total (each pair counted twice, as in eq. 13)
  covs <- as.data.frame(as.table(cov_mat)) |>
    dplyr::rename(group_1 = Var1, group_2 = Var2, covariance = Freq) |>
    dplyr::filter(as.character(group_1) < as.character(group_2)) |>
    dplyr::mutate(bqe_label = bqe_label, pct_of_total = 100 * 2 * covariance / total, .before = 1)

  list(shares = shares, covs = covs)
}

# Stacked bar of the marginal partition per BQE
plot_variance_partitioning <- function(shares) {
  component_order <- intersect(names(component_colours), unique(shares$component))
  shares |>
    dplyr::mutate(component = factor(component, levels = rev(component_order))) |>
    ggplot(aes(x = pct_marginal, y = bqe_label, fill = component)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, colour = "grey30") +
    scale_fill_manual(values = component_colours, breaks = component_order) +
    labs(x = "% of total latent variance", y = NULL, fill = "Component") +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.major.y = element_blank()) +
    guides(fill = guide_legend(nrow = 2, reverse = TRUE))
}

# Partition every BQE, write Table S9 and the covariances, and save Fig. 11
run_variance_partitioning <- function(results, table_dir, plot_dir = "plots/model_results") {
  dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

  vp <- purrr::imap(results, \(r, bqe) partition_marginal(r$fit, bqe))
  shares <- purrr::map(vp, "shares") |> purrr::list_rbind()
  covs <- purrr::map(vp, "covs") |> purrr::list_rbind()

  # Shares must sum to 100%; negative shares mean a group offsets variation from others
  shares |>
    dplyr::summarise(total = sum(pct_marginal), .by = bqe_label) |>
    print()

  # Table S9 with BQEs as columns
  shares_wide <- shares |>
    dplyr::select(bqe_label, component, pct_marginal) |>
    tidyr::pivot_wider(names_from = bqe_label, values_from = pct_marginal) |>
    dplyr::arrange(match(component, names(component_colours)))
  write.csv(shares_wide, file.path(table_dir, "variance_partitioning_marginal_summary.csv"), row.names = FALSE)
  write.csv(covs, file.path(table_dir, "variance_partitioning_group_covariances.csv"), row.names = FALSE)

  ggsave(file.path(plot_dir, "variance_partitioning_marginal_stacked.png"),
         plot_variance_partitioning(shares), width = 10, height = 6, dpi = 150)
  invisible(list(shares = shares, covs = covs))
}
