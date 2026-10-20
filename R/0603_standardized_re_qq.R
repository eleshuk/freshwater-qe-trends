# Standardized random-effect QQ plots for the river CLMMs: thesis Fig. S6
# Each conditional mode is divided by its expected SD under the model, sqrt(sigma2 - condVar),
# so if the normal random-effect assumption holds the points follow the 1:1 line
# Needs R/0600_model_helpers.R and R/theme_thesis.R sourced and ggplot2 loaded

# Raw and standardized conditional modes plus group sizes for every grouping factor in one fit
# Var(mode) = sigma2 - E[condVar] (law of total variance), so mode / sqrt(sigma2 - condVar) is ~ N(0, 1)
standardize_re <- function(fit) {
  modes <- ordinal::ranef(fit)
  cond_var <- ordinal::condVar(fit)
  re_var <- purrr::map_dbl(ordinal::VarCorr(fit), \(v) v[1, 1])
  purrr::imap(modes, \(m, g) {
    cv <- cond_var[[g]][, 1]
    n_obs <- table(fit$model[[g]])
    tibble::tibble(
      group = g,
      level = rownames(m),
      mode = m[, 1],
      z = m[, 1] / sqrt(pmax(re_var[[g]] - cv, 1e-8)),
      n_obs = as.integer(n_obs[rownames(m)])
    )
  }) |>
    purrr::list_rbind()
}

# Standardized QQ plot against the 1:1 line (not qqline), BQE in columns and random effect in rows
plot_re_qq <- function(re_df, title = NULL) {
  ggplot(re_df, aes(sample = z)) +
    stat_qq(size = 0.3) +
    geom_abline(slope = 1, intercept = 0, colour = "red") +
    # Wrap long BQE names so strips fit five columns at page width
    facet_grid(group ~ bqe_short, labeller = labeller(bqe_short = label_wrap_gen(width = 12))) +
    labs(title = title, x = "Theoretical quantiles", y = "Standardized conditional mode") +
    theme_thesis()
}

# Standardize the modes of every saved fit, write the tail summary and save the QQ and shrinkage plots
run_standardized_re_qq <- function(results_dir, out_dir = file.path(results_dir, "fit_assessment", "standardized_re"),
                                   plot_dir = "plots/model_assessment") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Standardized modes for each BQE, loading one fit at a time
  re_all <- list_fit_files(results_dir) |>
    purrr::map(\(path) {
      cat("Standardizing", bqe_from_path(path), "\n")
      fit <- load_fit(path)
      res <- standardize_re(fit) |>
        dplyr::mutate(bqe = bqe_from_path(path), .before = 1)
      rm(fit)
      gc()
      res
    }) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      bqe_short = sub("^QE[0-9-]+ - ", "", bqe),
      group = factor(group, levels = c("waterBody", "basin", "country"), labels = c("Water body", "Basin", "Country"))
    )

  # Tail summary; under N(0, 1): sd 1, 4.6% beyond |z| > 2, 0.27% beyond |z| > 3, excess kurtosis 0
  re_summary <- re_all |>
    dplyr::summarise(
      n_levels = dplyr::n(),
      sd_z = sd(z),
      pct_abs_z_gt2 = 100 * mean(abs(z) > 2),
      pct_abs_z_gt3 = 100 * mean(abs(z) > 3),
      excess_kurtosis = mean((z - mean(z))^4) / var(z)^2 - 3,
      pct_single_obs = 100 * mean(n_obs == 1),
      .by = c(bqe, group)
    )
  write.csv(re_summary, file.path(out_dir, "standardized_re_summary.csv"), row.names = FALSE)
  print(re_summary, n = Inf)

  # QQ plot at page width
  save_doc_plot(plot_re_qq(re_all), file.path(plot_dir, "standardized_re_qq.png"), height = 5)

  # Raw water body modes by number of observations: shows the shrinkage behind the S-shaped raw QQ plots
  p_nobs <- re_all |>
    dplyr::filter(group == "Water body") |>
    dplyr::mutate(n_bin = cut(n_obs, c(0, 1, 2, 3, 5, Inf), labels = c("1", "2", "3", "4-5", "6+"))) |>
    ggplot(aes(n_bin, mode)) +
    geom_boxplot(outlier.size = 0.2) +
    facet_wrap(~bqe_short, nrow = 1) +
    labs(
      title = "Water body conditional modes by number of observations",
      x = "Observations per water body", y = "Conditional mode (latent scale)"
    ) +
    theme_bw(base_size = 10)
  ggsave(file.path(plot_dir, "waterbody_mode_by_nobs.png"), p_nobs, width = 14, height = 6, dpi = 150)

  invisible(re_summary)
}
