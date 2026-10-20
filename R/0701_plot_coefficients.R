# Driver coefficients across the river BQE models: thesis Fig. 10 (plot) and Table S8 (table)
# Needs R/0600_model_helpers.R sourced and ggplot2 loaded

# Short BQE labels for the panels
coef_bqe_labels <- c(
  "QE1-2 - Other aquatic flora" = "Other aquatic flora",
  "QE1-2-3 - Macrophytes" = "Macrophytes",
  "QE1-2-4 - Phytobenthos" = "Phytobenthos",
  "QE1-3 - Benthic invertebrates" = "Benthic invertebrates",
  "QE1-4 - Fish" = "Fish"
)

# Readable labels for each model variable, with transformations noted
driver_labels <- c(
  pressure_hydromorphology = "Hydromorphology (P4)",
  pressure_diffuse_sources = "Diffuse sources (P2)",
  prop_agricultural = "Agricultural land (proportion)",
  log_imperv_pct = "Imperviousness (%, log)",
  amber_n_total_log = "Barrier count (log)",
  pressure_anthropogenic_unknown = "Anthropogenic, unknown (P8)",
  pressure_point_sources = "Point sources (P1)",
  pressure_introduced_species_litter = "Introduced species and litter (P5)",
  bio05 = "Max. temp. of warmest month (bio05)",
  pressure_abstraction = "Abstraction (P3)",
  bio04 = "Temp. seasonality (bio04)",
  sqrt_bio18 = "Precip. of warmest quarter (bio18, sqrt)",
  log_hu_deficit_coldest_qtr = "Humidity deficit, coldest quarter (log)",
  log_bio15 = "Precip. seasonality (bio15, log)",
  p_surplus_w = "Phosphorus surplus (winsorised)",
  log_hu_deficit_driest_qtr = "Humidity deficit, driest quarter (log)",
  pressure_anthropogenic_other = "Anthropogenic, other (P7)",
  log_prop_wetlands = "Wetlands (proportion, log)"
)

# Driver coefficients of every BQE with 95% Wald intervals (thresholds, cycle and region terms excluded)
coefficient_table <- function(results) {
  purrr::imap(results, \(r, bqe) dplyr::mutate(r$coef_table, bqe = bqe, .before = 1)) |>
    purrr::list_rbind() |>
    dplyr::filter(
      !grepl("\\|", variable),
      !grepl("^cycle", variable),
      !grepl("region", variable, ignore.case = TRUE)
    ) |>
    dplyr::mutate(
      bqe_label = unname(coef_bqe_labels[bqe]),
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se
    )
}

# Coefficient plot with one panel per BQE, drivers ordered by their largest |z| across BQEs
plot_coefficients <- function(coef_all) {
  var_order <- coef_all |>
    dplyr::summarise(max_abs_z = max(abs(z_value), na.rm = TRUE), .by = variable) |>
    dplyr::arrange(dplyr::desc(max_abs_z)) |>
    dplyr::pull(variable)

  # Flag coefficients whose 95% CI excludes zero and mark binary vs continuous drivers
  plot_data <- coef_all |>
    dplyr::mutate(
      variable = factor(variable, levels = rev(var_order)),
      var_type = dplyr::if_else(grepl("^pressure_", variable), "Binary (0 → 1)", "Continuous (per 2 SD)"),
      significant = dplyr::if_else(ci_lower > 0 | ci_upper < 0, "CI excludes 0", "CI includes 0")
    )

  ggplot(plot_data, aes(x = estimate, y = variable, colour = significant, shape = var_type)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_pointrange(aes(xmin = ci_lower, xmax = ci_upper), size = 0.3, linewidth = 0.4) +
    facet_wrap(~ bqe_label, nrow = 1) +
    scale_y_discrete(labels = driver_labels) +
    # Fewer, shorter tick labels so neighbouring panels don't collide
    scale_x_continuous(breaks = c(-0.5, 0, 0.5, 1, 1.5), labels = c("-0.5", "0", "0.5", "1", "1.5")) +
    scale_colour_manual(values = c("CI excludes 0" = "black", "CI includes 0" = "darkgray")) +
    scale_shape_manual(values = c("Binary (0 → 1)" = 17, "Continuous (per 2 SD)" = 16)) +
    labs(x = "Coefficient estimate (probit)", y = NULL, colour = NULL, shape = "Coefficient type") +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 13),
      axis.text.y = element_text(size = 16),
      axis.title.x = element_text(size = 16),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16),
      strip.text = element_text(size = 14),
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.major.y = element_line(colour = "grey92"),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey95"),
      plot.margin = margin(t = 5.5, r = 20, b = 5.5, l = 5.5)
    )
}

# Write Table S8 and save Fig. 10
run_coefficient_figure <- function(results, table_dir, plot_dir = "plots/model_results") {
  dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

  coef_all <- coefficient_table(results)
  write.csv(coef_all, file.path(table_dir, "driver_coefficients.csv"), row.names = FALSE)

  p <- plot_coefficients(coef_all)
  ggsave(file.path(plot_dir, "coefficient_comparison_bqe_facet.png"), p, width = 14, height = 8, dpi = 200)
  invisible(coef_all)
}
