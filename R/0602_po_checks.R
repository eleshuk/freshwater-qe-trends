# Parallel slopes (proportional odds) check for the river CLMMs: thesis Table S7
# Fits one binary probit per cut point and compares it with the pooled ordinal model, using the saved model frames
# Needs R/0600_model_helpers.R sourced and ggplot2 loaded

# Cut point k splits status into Y <= k vs Y > k
cut_labels <- c(
  "0" = "Pooled (clm, one coefficient)",
  "1" = "1: High | Good or worse",
  "2" = "2: Good or better | Moderate or worse",
  "3" = "3: Moderate or better | Poor or worse",
  "4" = "4: Poor or better | Bad"
)

# Turn a coefficient matrix into a tibble
coef_tibble <- function(co, flip = FALSE) {
  sgn <- if (flip) -1 else 1
  tibble::tibble(term = rownames(co), estimate = sgn * co[, 1], se = co[, 2])
}

# Fit the pooled model and one binary probit per cut point for one BQE
check_bqe <- function(path) {
  bqe <- bqe_from_path(path)
  cat("Checking", bqe, "\n")
  fit <- load_fit(path)
  mf <- fit$model
  resp <- names(mf)[1]
  y <- as.integer(mf[[resp]])
  
  # Fixed-effects terms only (glm cannot take the random effects)
  tl <- attr(terms(fit$formula), "term.labels")
  fixed_terms <- tl[!grepl("\\|", tl)]
  driver_terms <- names(fit$beta)[!grepl("cycle|region", names(fit$beta))]
  
  # Pooled reference: fixed-effects clm, one coefficient per driver across all cuts
  fit_clm <- ordinal::clm(reformulate(fixed_terms, response = resp), data = mf, link = "probit")
  pooled <- coef_tibble(summary(fit_clm)$coefficients) |>
    dplyr::mutate(cut = 0L)
  
  # One binary probit per cut; sign flipped so positive = worse status, as in the clmm
  by_cut <- purrr::map(1:4, \(k) {
    d <- dplyr::mutate(mf, y_bin = as.integer(y <= k))
    g <- glm(reformulate(fixed_terms, response = "y_bin"), data = d, family = binomial(link = "probit"))
    coef_tibble(summary(g)$coefficients, flip = TRUE) |>
      dplyr::mutate(cut = k)
  }) |>
    purrr::list_rbind()
  
  rm(fit, fit_clm, mf)
  gc()
  dplyr::bind_rows(pooled, by_cut) |>
    dplyr::filter(term %in% driver_terms) |>
    dplyr::mutate(bqe = sub("^QE[0-9-]+ - ", "", bqe), .before = 1)
}

# Check every saved fit and write the tables to out_dir and the figure to plot_dir
run_po_checks <- function(results_dir, out_dir = file.path(results_dir, "po_check"),
                          plot_dir = "plots/model_assessment") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  po_long <- purrr::map(list_fit_files(results_dir), check_bqe) |>
    purrr::list_rbind()

  # Wide table; cuts 2 and 3 are the P(Good or better) and P(Poor or Bad) splits used in the Q3 plots
  po_wide <- po_long |>
    dplyr::select(bqe, term, cut, estimate) |>
    tidyr::pivot_wider(names_from = cut, values_from = estimate, names_prefix = "cut_") |>
    dplyr::rename(pooled = cut_0) |>
    dplyr::mutate(
      sign_flip_2_3 = sign(cut_2) != sign(cut_3),
      ratio_2_3 = pmax(abs(cut_2), abs(cut_3)) / pmin(abs(cut_2), abs(cut_3)),
      flag = sign_flip_2_3 | ratio_2_3 > 2
    )

  write.csv(po_long, file.path(out_dir, "po_check_long.csv"), row.names = FALSE)
  write.csv(po_wide, file.path(out_dir, "po_check_wide.csv"), row.names = FALSE)

  # Flagged drivers only
  po_wide |>
    dplyr::filter(flag) |>
    print(n = Inf)

  # Plot: one row per driver, one point per cut, pooled estimate as a black bar
  plot_df <- po_long |>
    dplyr::mutate(
      cut_label = factor(cut_labels[as.character(cut)], levels = cut_labels),
      term = factor(term, levels = rev(unique(term)))
    )

  p <- ggplot(dplyr::filter(plot_df, cut > 0), aes(x = estimate, y = term, colour = cut_label)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_pointrange(
      aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se),
      position = position_dodge(width = 0.6), size = 0.2
    ) +
    geom_point(data = dplyr::filter(plot_df, cut == 0), colour = "black", shape = "|", size = 4) +
    facet_wrap(~bqe, nrow = 1) +
    scale_colour_viridis_d(option = "D", end = 0.9) +
    labs(
      title = "Proportional odds check: driver coefficients by cut point",
      subtitle = "Binary probit per cut (fixed effects only); black bar = pooled clm. Similar values across cuts = assumption holds",
      x = "Coefficient (probit, positive = worse status)", y = NULL, colour = "Cut point"
    ) +
    theme_bw() +
    theme(legend.position = "bottom", legend.direction = "vertical")

  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
  ggsave(file.path(plot_dir, "po_check_coefficients.png"), p, width = 16, height = 8, dpi = 200)

  invisible(list(long = po_long, wide = po_wide))
}
