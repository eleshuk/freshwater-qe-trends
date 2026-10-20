# Fit assessment for the river CLMMs: AIC against simpler models, weighted kappa and observed vs predicted classes
# Thesis Tables S5-S6 and Fig. S7; reads the saved fit_<bqe>.RData files and refits only fast clm baselines
# Needs R/0600_model_helpers.R and R/theme_thesis.R sourced and ggplot2 loaded

# Cohen's kappa with unweighted, linear or quadratic disagreement weights
weighted_kappa <- function(obs, pred, k, type = "quadratic") {
  o <- table(factor(obs, levels = 1:k), factor(pred, levels = 1:k)) / length(obs)
  e <- outer(rowSums(o), colSums(o))
  d <- abs(outer(1:k, 1:k, "-"))
  w <- switch(type,
              unweighted = (d > 0) * 1,
              linear = d / (k - 1),
              quadratic = d^2 / (k - 1)^2
  )
  1 - sum(w * o) / sum(w * e)
}

# Kappa and accuracy for one prediction level
score_level <- function(obs, probs, k, level) {
  pred <- max.col(probs, ties.method = "first")
  tibble::tibble(
    level = level,
    kappa_quadratic = weighted_kappa(obs, pred, k, "quadratic"),
    kappa_linear = weighted_kappa(obs, pred, k, "linear"),
    kappa_unweighted = weighted_kappa(obs, pred, k, "unweighted"),
    accuracy = mean(obs == pred),
    within_one_class = mean(abs(obs - pred) <= 1)
  )
}

# Assess one BQE: AIC against fast clm baselines on the same rows, then kappa at three prediction levels
assess_bqe <- function(path, out_dir) {
  bqe <- bqe_from_path(path)
  cat("Assessing", bqe, "\n")
  fit <- load_fit(path)
  mf <- fit$model
  resp <- names(mf)[1]
  
  # Fixed-effects formula: drop the (1 | group) terms
  tl <- attr(terms(fit$formula), "term.labels")
  fixed_form <- reformulate(tl[!grepl("\\|", tl)], response = resp)
  
  # Baselines fitted on exactly the same rows
  fit_null <- ordinal::clm(reformulate("1", response = resp), data = mf, link = "probit")
  fit_fixed <- ordinal::clm(fixed_form, data = mf, link = "probit")
  
  aic_tab <- tibble::tibble(
    bqe = bqe,
    n_obs = nobs(fit),
    model = c("null (thresholds only)", "fixed effects only (clm)", "full CLMM"),
    edf = c(attr(logLik(fit_null), "df"), attr(logLik(fit_fixed), "df"), attr(logLik(fit), "df")),
    logLik = c(as.numeric(logLik(fit_null)), as.numeric(logLik(fit_fixed)), as.numeric(logLik(fit))),
    AIC = c(AIC(fit_null), AIC(fit_fixed), AIC(fit))
  ) |>
    dplyr::mutate(delta_AIC = AIC - min(AIC))
  
  # Fixed part of the linear predictor (subset columns to surviving coefficients, session 13 fix)
  mm <- model.matrix(fit)[, names(fit$beta), drop = FALSE]
  xb <- as.numeric(mm %*% fit$beta)
  
  # Random-effect modes mapped to rows, and variances, by grouping factor
  groups <- names(fit$ST)
  re <- ordinal::ranef(fit)
  re_rows <- purrr::map(groups, \(g) re[[g]][as.character(mf[[g]]), 1]) |> purrr::set_names(groups)
  re_var <- purrr::map_dbl(ordinal::VarCorr(fit), \(v) v[1, 1])
  wb <- groups[grepl("waterbody", groups, ignore.case = TRUE)]
  
  theta <- as.numeric(fit$Theta)
  k <- length(theta) + 1
  obs <- as.integer(mf[[resp]])
  
  # Level 1: fixed effects only, averaged over all random effects (new water body, basin and country)
  p_marg <- class_probs(xb, theta, sqrt(1 + sum(re_var)))
  # Level 2: country and basin effects known, averaged over the water body effect (new water body)
  eta_cb <- xb + Reduce(`+`, re_rows[setdiff(groups, wb)])
  p_cb <- class_probs(eta_cb, theta, sqrt(1 + re_var[[wb]]))
  # Level 3: all random-effect modes (in-sample, optimistic)
  p_cond <- class_probs(xb + Reduce(`+`, re_rows), theta, 1)
  
  # Observed vs mean predicted class proportions at levels 1 and 2
  class_prop <- tibble::tibble(
    bqe = bqe,
    status = 1:k,
    observed = as.numeric(prop.table(table(factor(obs, levels = 1:k)))),
    predicted_marginal = colMeans(p_marg),
    predicted_new_wb = colMeans(p_cb)
  )
  
  kappa_tab <- dplyr::bind_rows(
    score_level(obs, p_marg, k, "1 fixed effects only (marginal)"),
    score_level(obs, p_cb, k, "2 + country and basin (new water body)"),
    score_level(obs, p_cond, k, "3 + water body (conditional, in-sample)")
  ) |>
    dplyr::mutate(bqe = bqe, n_obs = length(obs), .before = 1)
  
  # Confusion matrices for the marginal and new-water-body levels
  conf <- purrr::map(
    list(marginal = p_marg, new_waterbody = p_cb),
    \(p) as.data.frame.matrix(table(
      observed = factor(obs, levels = 1:k),
      predicted = factor(max.col(p, ties.method = "first"), levels = 1:k)
    ))
  )
  purrr::iwalk(conf, \(m, nm) write.csv(m, file.path(out_dir, paste0("confusion_", nm, "_", bqe, ".csv"))))
  
  rm(fit, fit_null, fit_fixed, mf, mm)
  gc()
  list(aic = aic_tab, kappa = kappa_tab, class_prop = class_prop)
}

# Plot observed vs predicted class proportions per BQE and save at page width
plot_obs_pred <- function(class_prop_all, plot_dir, height = 6) {
  wfd_labels <- c("High", "Good", "Moderate", "Poor", "Bad")
  type_labels <- c(
    observed = "Observed",
    predicted_marginal = "Predicted (fixed effects only)",
    predicted_new_wb = "Predicted (+ country and basin)"
  )
  
  # Long format with short BQE names
  plot_df <- class_prop_all |>
    tidyr::pivot_longer(
      cols = c(observed, predicted_marginal, predicted_new_wb),
      names_to = "type",
      values_to = "proportion"
    ) |>
    dplyr::mutate(
      type = factor(type_labels[type], levels = type_labels),
      status = factor(wfd_labels[status], levels = wfd_labels),
      bqe_short = sub("^QE[0-9-]+ - ", "", bqe)
    )
  
  # Grouped barplot per BQE
  p <- ggplot(plot_df, aes(x = status, y = proportion, fill = type)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    facet_wrap(~ bqe_short, ncol = 3, scales = "free_y") +
    scale_fill_manual(values = c("grey40", "steelblue", "coral")) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "Quality element status", y = "Proportion", fill = NULL) +
    theme_thesis() +
    theme(
      legend.position = "top",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  save_doc_plot(p, file.path(plot_dir, "obs_vs_pred_class_prop.png"), height = height)
  invisible(p)
}

# Count countries per basin in the rows each saved fit was fitted on
check_cross_border <- function(path) {
  bqe <- bqe_from_path(path)
  mf <- load_fit(path)$model
  
  basin_countries <- mf |>
    dplyr::distinct(basin, country) |>
    dplyr::count(basin, name = "n_countries")
  
  # Share of rows that sit in a cross-border basin
  cross_basins <- basin_countries |>
    dplyr::filter(n_countries > 1) |>
    dplyr::pull(basin)
  
  tibble::tibble(
    bqe = bqe,
    n_basins = nrow(basin_countries),
    n_cross_border = length(cross_basins),
    pct_basins_cross_border = 100 * length(cross_basins) / nrow(basin_countries),
    pct_rows_cross_border = 100 * mean(mf$basin %in% cross_basins),
    max_countries_per_basin = max(basin_countries$n_countries)
  )
}

# Assess every saved fit and write the tables to out_dir and the figure to plot_dir
run_fit_assessment <- function(results_dir, out_dir = file.path(results_dir, "fit_assessment"),
                               plot_dir = "plots/model_assessment") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  fit_files <- list_fit_files(results_dir)
  res <- purrr::map(fit_files, \(p) assess_bqe(p, out_dir))

  # Combine and save AIC and kappa tables
  aic_all <- purrr::map(res, "aic") |> purrr::list_rbind()
  kappa_all <- purrr::map(res, "kappa") |> purrr::list_rbind()
  write.csv(aic_all, file.path(out_dir, "aic_comparison.csv"), row.names = FALSE)
  write.csv(kappa_all, file.path(out_dir, "kappa_by_level.csv"), row.names = FALSE)
  print(aic_all, n = Inf)
  print(kappa_all, n = Inf)

  # Combine and save class proportions, then plot them
  class_prop_all <- purrr::map(res, "class_prop") |> purrr::list_rbind()
  write.csv(class_prop_all, file.path(out_dir, "class_prop_obs_vs_pred.csv"), row.names = FALSE)
  plot_obs_pred(class_prop_all, plot_dir)

  # Cross-border basin check
  cross_border <- purrr::map(fit_files, check_cross_border) |> purrr::list_rbind()
  write.csv(cross_border, file.path(out_dir, "cross_border_basins.csv"), row.names = FALSE)

  invisible(list(aic = aic_all, kappa = kappa_all, class_prop = class_prop_all, cross_border = cross_border))
}
