# Status over time and by biogeographical region for the river BQEs: thesis Fig. 7, 8, 9 and 12
# Fig. 7 shows observed proportions; Fig. 8, 9 and 12 show model predictions averaged over the random effects and over
# the observed assessments, each keeping its recorded drivers, pressures and region, with only the cycle changed
# Needs R/0600_model_helpers.R and R/theme_thesis.R sourced and ggplot2 loaded

# Short BQE labels for the panels
temporal_bqe_labels <- c(
  "QE1-2 - Other aquatic flora" = "Other flora",
  "QE1-2-3 - Macrophytes" = "Macrophytes",
  "QE1-2-4 - Phytobenthos" = "Phytobenthos",
  "QE1-3 - Benthic invertebrates" = "Benthic Invertebrates",
  "QE1-4 - Fish" = "Fish"
)

# Status labels and WFD standard colours
status_labels <- c("1" = "High", "2" = "Good", "3" = "Moderate", "4" = "Poor", "5" = "Bad")
wfd_colours <- c("High" = "#0000FF", "Good" = "#00A000", "Moderate" = "#E6D800", "Poor" = "#FF8C00", "Bad" = "#FF0000")
transition_levels <- c("2010→2016", "2016→2022", "2010→2022")

# Observed river records with the same filters as the model fits, for the fitted BQEs only
read_observed_status <- function(data_path, bqe_names) {
  arrow::read_parquet(data_path) |>
    dplyr::filter(
      surfaceWaterBodyCategory == "RW",
      !countryCode %in% c("CY", "MT"),
      as.character(qeCode) %in% bqe_names,
      !is.na(qeStatusOrPotentialValue)
    ) |>
    dplyr::transmute(
      bqe_code = stringr::str_extract(as.character(qeCode), "QE[\\d-]+"),
      bqe_label = unname(temporal_bqe_labels[as.character(qeCode)]),
      status_int = as.character(as.integer(as.character(qeStatusOrPotentialValue))),
      status_label = factor(status_labels[status_int], levels = c("Bad", "Poor", "Moderate", "Good", "High")),
      cycle_label = as.character(cYear),
      region_label = stringr::str_to_title(biogeographical_region)
    )
}

# Number of observed records in each BQE x cycle x region cell; predictions are only shown for cells with data
count_cells <- function(observed) {
  dplyr::count(observed, bqe_code, cycle_label, region_label, name = "cell_n")
}

# Fig. 7: observed status shares by cycle and region, faded where a cell has fewer than min_n records
plot_observed_status_by_region <- function(observed, min_n = 30) {
  obs_region <- observed |>
    dplyr::filter(!is.na(region_label)) |>
    dplyr::count(bqe_label, cycle_label, region_label, status_label) |>
    dplyr::mutate(proportion = n / sum(n), cell_n = sum(n), .by = c(bqe_label, cycle_label, region_label))
  obs_region_n <- dplyr::distinct(obs_region, bqe_label, cycle_label, region_label, cell_n)

  ggplot(obs_region, aes(x = cycle_label, y = proportion, fill = status_label)) +
    geom_col(aes(alpha = cell_n < min_n), position = "stack", width = 0.7) +
    geom_text(data = obs_region_n, aes(x = cycle_label, y = 1.03, label = cell_n),
              inherit.aes = FALSE, size = 2.2, vjust = 0) +
    facet_grid(region_label ~ bqe_label) +
    scale_fill_manual(values = wfd_colours, guide = guide_legend(reverse = TRUE)) +
    scale_alpha_manual(values = c("FALSE" = 1, "TRUE" = 0.3), guide = "none") +
    scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, 0.25),
                       expand = expansion(mult = c(0, 0.12))) +
    labs(x = "Assessment cycle", y = "Proportion of observations", fill = "Status") +
    theme_thesis() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold", size = 7))
}

# Wald test of the change between cycles in each region on the latent scale (cycle plus cycle x region terms)
# Gives the significance stars in Fig. 9
cycle_effect_tests <- function(results) {
  purrr::imap(results, function(r, bqe) {
    V <- stats::vcov(r$fit)
    beta_names <- rownames(V)

    # Cycle and region levels from the coefficient names, with the reference levels first
    cycle_levels <- gsub("cycle", "", grep("^cycle\\d", beta_names, value = TRUE))
    cycle_levels <- cycle_levels[!grepl(":", cycle_levels)]
    region_levels <- gsub("^region", "", grep("^region[A-Za-z]", beta_names, value = TRUE))
    region_levels <- region_levels[!grepl(":", region_levels)]
    ref_region <- levels(r$fit$model$region)[1]
    ref_cycle <- if (all(c("2016", "2022") %in% cycle_levels)) "2010" else if ("2022" %in% cycle_levels) "2016" else cycle_levels[1]
    all_regions <- c(ref_region, region_levels)
    all_cycles <- sort(c(ref_cycle, cycle_levels))
    stopifnot(identical(names(r$fit$coefficients), beta_names[seq_along(r$fit$coefficients)]))
    b <- c(r$fit$coefficients, rep(0, length(beta_names) - length(r$fit$coefficients)))

    # Coefficient weights for cycle cy vs the reference cycle in region rg (NULL if a term was dropped)
    cycle_weights <- function(cy, rg) {
      w <- stats::setNames(numeric(length(beta_names)), beta_names)
      if (cy == ref_cycle) return(w)
      cycle_var <- paste0("cycle", cy)
      int_var <- paste0("cycle", cy, ":region", rg)
      if (!cycle_var %in% beta_names) return(NULL)
      w[cycle_var] <- 1
      if (rg != ref_region) {
        if (!int_var %in% beta_names) return(NULL)
        w[int_var] <- 1
      }
      w
    }

    # Consecutive cycles, plus first to last when there are more than two
    cycle_pairs <- purrr::map(seq_along(all_cycles)[-1], \(i) all_cycles[c(i - 1, i)])
    if (length(all_cycles) > 2) cycle_pairs <- c(cycle_pairs, list(all_cycles[c(1, length(all_cycles))]))

    # Change between each pair of cycles in each region, SE from the full vcov
    purrr::map(cycle_pairs, function(pair) {
      purrr::map(all_regions, function(rg) {
        w_now <- cycle_weights(pair[2], rg)
        w_prev <- cycle_weights(pair[1], rg)
        if (is.null(w_now) || is.null(w_prev)) return(NULL)
        w <- w_now - w_prev
        est <- sum(w * b)
        se <- sqrt(as.numeric(t(w) %*% V %*% w))
        p <- 2 * stats::pnorm(-abs(est / se))
        data.frame(
          bqe = bqe, bqe_label = unname(temporal_bqe_labels[bqe]),
          transition = paste0(pair[1], "→", pair[2]),
          region_label = stringr::str_to_title(rg),
          total_effect = est, se = se, z_value = est / se, p_value = p,
          sig = dplyr::case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", .default = "")
        )
      }) |>
        purrr::list_rbind()
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()
}

# Point estimate and 95% percentile interval from a vector of draws (element 1 = estimate)
summarise_draws <- function(x) {
  data.frame(
    estimate = x[1],
    ci_lower = stats::quantile(x[-1], 0.025, names = FALSE),
    ci_upper = stats::quantile(x[-1], 0.975, names = FALSE)
  )
}

# Mean cumulative probability P(Y <= k) per region and cycle, averaged over that region's assessments
# Draw 1 is the point estimate, the others are joint draws of the coefficients and RE SDs from vcov(fit)
cum_probs_observed <- function(fit, n_sim = 2000, chunk_size = 5000) {
  mf <- fit$model
  vc <- stats::vcov(fit)
  par_hat <- c(fit$coefficients, vapply(fit$ST, function(m) m[1, 1], numeric(1)))
  stopifnot(length(par_hat) == nrow(vc))
  names(par_hat) <- rownames(vc)
  draws <- rbind(par_hat, MASS::mvrnorm(n_sim, mu = par_hat, Sigma = vc))

  # Marginal over the water body, basin and country REs
  sd_cols <- utils::tail(seq_along(par_hat), length(fit$ST))
  denom <- sqrt(1 + rowSums(draws[, sd_cols, drop = FALSE]^2))
  theta <- draws[, names(fit$alpha), drop = FALSE]
  beta_names <- names(fit$beta)
  beta_draws <- t(draws[, beta_names, drop = FALSE])

  # Fixed-effects formula without the (1 | group) terms
  tl <- attr(stats::terms(fit$formula), "term.labels")
  fixed_form <- stats::reformulate(tl[!grepl("\\|", tl)])

  regions <- levels(droplevels(mf$region))
  region_n <- as.numeric(table(factor(mf$region, levels = regions)))
  chunks <- split(seq_len(nrow(mf)), ceiling(seq_len(nrow(mf)) / chunk_size))
  cycles <- levels(mf$cycle)

  # Same assessments with the cycle set to each cycle in turn
  cum <- purrr::map(cycles, function(cy) {
    mf_cy <- mf
    mf_cy$cycle <- factor(cy, levels = cycles)
    x <- stats::model.matrix(fixed_form, data = mf_cy)[, beta_names, drop = FALSE]
    reg <- factor(mf$region, levels = regions)

    # Sum P(Y <= k) over assessments within each region, in chunks to limit memory
    totals <- array(0, dim = c(length(regions), nrow(draws), ncol(theta)),
                    dimnames = list(regions, NULL, colnames(theta)))
    for (idx in chunks) {
      eta <- x[idx, , drop = FALSE] %*% beta_draws
      for (k in seq_len(ncol(theta))) {
        z <- sweep(sweep(-eta, 2, theta[, k], "+"), 2, denom, "/")
        s <- rowsum(stats::pnorm(z), as.character(reg[idx]))
        totals[rownames(s), , k] <- totals[rownames(s), , k] + s
      }
    }
    totals / region_n
  }) |>
    purrr::set_names(cycles)

  list(cum = cum, region_n = stats::setNames(region_n, regions))
}

# Class probabilities, P(Good or better) changes by region and the overall change since the first cycle for one BQE
predict_bqe <- function(fit, bqe, n_sim = 2000, chunk_size = 5000) {
  message("Predicting ", bqe)
  cp <- cum_probs_observed(fit, n_sim, chunk_size)
  regions <- names(cp$region_n)
  cycles <- names(cp$cum)
  n_k <- dim(cp$cum[[1]])[3]

  # Class probabilities per region and cycle (point estimates) for the stacked bars
  classes <- purrr::imap(cp$cum, function(a, cy) {
    purrr::map(regions, function(rg) {
      data.frame(cycle = cy, region = rg, status_int = as.character(seq_len(n_k + 1)),
                 probs = diff(c(0, a[rg, 1, ], 1)))
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()

  # Change in P(Good or better) = P(Y <= 2) between cycles in each region: consecutive cycles and first to last
  cycle_pairs <- c(
    purrr::map(seq_along(cycles)[-1], \(i) cycles[c(i - 1, i)]),
    if (length(cycles) > 2) list(cycles[c(1, length(cycles))])
  )
  change_region <- purrr::map(cycle_pairs, function(pr) {
    purrr::map(regions, function(rg) {
      data.frame(region = rg, from_cycle = pr[1], to_cycle = pr[2],
                 summarise_draws(cp$cum[[pr[2]]][rg, , "2|3"] - cp$cum[[pr[1]]][rg, , "2|3"]))
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()

  # P(Good or better) over all assessments, i.e. the region means weighted by each region's number of assessments
  w <- cp$region_n / sum(cp$region_n)
  p_overall <- purrr::map(cp$cum, \(a) colSums(matrix(a[, , "2|3"], nrow = length(regions)) * w))
  change_overall <- purrr::imap(p_overall, \(x, cy) data.frame(cycle = cy, summarise_draws(x - p_overall[[1]]))) |>
    purrr::list_rbind()

  list(classes = classes, change_region = change_region, change_overall = change_overall) |>
    purrr::map(\(d) dplyr::mutate(d, bqe = bqe, bqe_label = unname(temporal_bqe_labels[bqe]),
                                  bqe_code = stringr::str_extract(bqe, "QE[\\d-]+"), .before = 1))
}

# Fig. 8: predicted status shares by cycle and region
plot_predicted_status_by_region <- function(pred_classes, min_n = 30) {
  pred_region_n <- dplyr::distinct(pred_classes, bqe_label, cycle_label, region_label, cell_n)

  ggplot(pred_classes, aes(x = cycle_label, y = probs, fill = status_label)) +
    geom_col(aes(alpha = cell_n < min_n), position = "stack", width = 0.7) +
    geom_text(data = pred_region_n, aes(x = cycle_label, y = 1.03, label = cell_n),
              inherit.aes = FALSE, size = 2.2, vjust = 0) +
    facet_grid(region_label ~ bqe_label) +
    scale_fill_manual(values = wfd_colours, guide = guide_legend(reverse = TRUE)) +
    scale_alpha_manual(values = c("FALSE" = 1, "TRUE" = 0.3), guide = "none") +
    scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, 0.25),
                       expand = expansion(mult = c(0, 0.12))) +
    labs(x = "Assessment cycle", y = "Predicted probability", fill = "Status") +
    theme_thesis() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold", size = 7))
}

# Fig. 9: change in P(Good or High) between cycles by BQE and region, with significance stars
plot_change_by_region <- function(delta_sig) {
  bar_dodge <- position_dodge2(width = 0.9, preserve = "single", padding = 0.1)

  ggplot(delta_sig, aes(x = bqe_label, y = change, fill = transition)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_col(position = bar_dodge, width = 0.9) +
    geom_text(aes(label = sig, vjust = ifelse(change >= 0, -0.2, 1.2), group = transition),
              position = bar_dodge, size = 2.6) +
    facet_wrap(~ region_label, ncol = 3, scales = "free_y") +
    scale_y_continuous(labels = scales::label_number(accuracy = 1, scale = 100, suffix = " pp"),
                       expand = expansion(mult = 0.18)) +
    scale_fill_manual(values = c("#4393C3", "#D6604D", "#762A83")) +
    labs(
      x = NULL, y = "Change in P(Good or High) (percentage points)", fill = "Period",
      caption = paste(
        "2010→2022 = direct change between the first and last cycle, estimated from the model.",
        "* p<0.05, ** p<0.01, *** p<0.001. Y-axis ranges differ between panels.",
        sep = "\n"
      )
    ) +
    theme_thesis() +
    theme(
      legend.position = "bottom", legend.margin = margin(0, 0, 0, 0), legend.text = element_text(size = 9),
      panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8), axis.text.y = element_text(size = 8),
      strip.text = element_text(face = "bold"), plot.caption = element_text(hjust = 0, size = 8)
    )
}

# Fig. 12: change in P(Good or High) since the first cycle, all BQEs on one panel
plot_change_since_first_cycle <- function(change_overall) {
  ggplot(change_overall, aes(x = cycle_year, y = estimate, colour = bqe_label, fill = bqe_label)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.1, colour = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = sort(unique(change_overall$cycle_year))) +
    expand_limits(y = -0.05) +
    scale_y_continuous(breaks = seq(-0.05, 0.2, by = 0.05),
                       labels = scales::label_number(scale = 100, suffix = " pp")) +
    labs(x = "Assessment cycle", y = "Change in P(Good or High status) since the first cycle",
         colour = NULL, fill = NULL) +
    theme_thesis() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
}

# Compute the predictions once and write the tables and Fig. 7, 8, 9 and 12
run_temporal_region_figures <- function(results, data_path, table_dir, plot_dir = "plots/model_results",
                                        n_sim = 2000, chunk_size = 5000, min_n = 30) {
  dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

  # Fig. 7 from the observed records
  observed <- read_observed_status(data_path, names(results))
  cells <- count_cells(observed)
  save_doc_plot(plot_observed_status_by_region(observed, min_n), file.path(plot_dir, "observed_status_by_region.png"), height = 8.5)
  rm(observed)

  # Latent-scale significance of the cycle changes
  tests <- cycle_effect_tests(results)
  write.csv(tests, file.path(table_dir, "total_cycle_effects_by_region.csv"), row.names = FALSE)

  # One prediction pass per BQE; the seed matches the thesis runs
  set.seed(42)
  pred <- purrr::imap(results, \(r, bqe) predict_bqe(r$fit, bqe, n_sim, chunk_size))
  bind_part <- \(part) purrr::map(pred, part) |> purrr::list_rbind()

  # Fig. 8: class probabilities, kept only for cells with observed data
  pred_classes <- bind_part("classes") |>
    dplyr::mutate(
      cycle_label = cycle,
      region_label = stringr::str_to_title(region),
      status_label = factor(status_labels[status_int], levels = c("Bad", "Poor", "Moderate", "Good", "High"))
    ) |>
    dplyr::inner_join(cells, by = c("bqe_code", "cycle_label", "region_label"))
  write.csv(pred_classes, file.path(table_dir, "cycle_x_region_status_observed.csv"), row.names = FALSE)
  save_doc_plot(plot_predicted_status_by_region(pred_classes, min_n), file.path(plot_dir, "cycle_x_region_status_observed.png"), height = 8.5)

  # Fig. 9: regional changes where both cycles have observed data, with stars from the Wald tests
  change_region <- bind_part("change_region") |>
    dplyr::rename(change = estimate) |>
    dplyr::mutate(region_label = stringr::str_to_title(region), transition = paste0(from_cycle, "→", to_cycle)) |>
    dplyr::semi_join(cells, by = c("bqe_code", from_cycle = "cycle_label", "region_label")) |>
    dplyr::semi_join(cells, by = c("bqe_code", to_cycle = "cycle_label", "region_label")) |>
    dplyr::left_join(dplyr::select(tests, bqe_label, region_label, transition, sig),
                     by = c("bqe_label", "region_label", "transition")) |>
    dplyr::mutate(sig = dplyr::coalesce(sig, ""), transition = factor(transition, levels = transition_levels))
  write.csv(change_region, file.path(table_dir, "p_good_or_better_change_by_region_observed.csv"), row.names = FALSE)
  save_doc_plot(plot_change_by_region(change_region), file.path(plot_dir, "rate_of_change_by_region_sig_observed.png"), height = 7.5)

  # Fig. 12: change since the first cycle over all assessments
  change_overall <- bind_part("change_overall") |>
    dplyr::mutate(cycle_year = as.numeric(cycle))
  write.csv(change_overall, file.path(table_dir, "p_good_or_better_change_since_first_cycle_observed.csv"), row.names = FALSE)
  save_doc_plot(plot_change_since_first_cycle(change_overall), file.path(plot_dir, "p_good_or_better_change_by_cycle_observed.png"), height = 5)

  invisible(list(tests = tests, classes = pred_classes, change_region = change_region, change_overall = change_overall))
}
