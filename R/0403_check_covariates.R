# Collinearity diagnostics for driver selection (rivers, transformed covariates): thesis Fig. S3-S5
# Input is rivers_model_df_transformed.parquet from 0402_transform_rivers_data.R; ggplot2 must be loaded
# Plots are saved in pixels at 150 dpi so they drop into Google Sheets at a readable size
# Widths are multiples of 100 px, so each image spans whole default-width (100 px) columns

# Save a plot as PNG with fixed pixel dimensions and a white background
save_plot <- function(p, output_dir, filename, width_px, height_px, dpi = 150) {
  ggsave(
    file.path(output_dir, filename), p,
    width = width_px, height = height_px, units = "px",
    dpi = dpi, bg = "white"
  )
}

# Shared theme so all plots look consistent side by side in a sheet
theme_sheet <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(size = base_size + 1, face = "bold"),
      plot.title.position = "plot",
      axis.text = element_text(size = base_size + 1, colour = "grey15"),
      panel.grid.minor = element_blank()
    )
}

# Lower-triangle correlation heatmap, ordered by the given order or by clustering
plot_cor_heatmap <- function(data, vars, title, order = NULL) {
  # Drop zero/near-zero variance columns that produce NaN correlations
  col_vars <- sapply(data[, vars, drop = FALSE], var, na.rm = TRUE)
  bad <- names(which(is.na(col_vars) | col_vars < 1e-10))
  if (length(bad) > 0) {
    cat("  Dropping zero-variance columns:", paste(bad, collapse = ", "), "\n")
    vars <- setdiff(vars, bad)
  }
  
  mat <- cor(data[, vars, drop = FALSE], use = "pairwise.complete.obs")
  
  # Use the given order, otherwise cluster by absolute correlation
  if (!is.null(order)) {
    vars_ordered <- intersect(order, vars)
  } else {
    dist_mat <- as.dist(1 - abs(mat))
    dist_mat[is.na(dist_mat)] <- 2
    vars_ordered <- vars[hclust(dist_mat)$order]
  }
  
  # Put the matrix in display order before masking so the triangle matches the axes
  mat <- mat[vars_ordered, vars_ordered]
  mat[upper.tri(mat, diag = TRUE)] <- NA
  
  cor_df <- as.data.frame(as.table(mat)) |>
    dplyr::rename(var1 = Var1, var2 = Var2, r = Freq) |>
    dplyr::filter(!is.na(r)) |>
    dplyr::mutate(
      var1 = factor(var1, levels = vars_ordered),
      var2 = factor(var2, levels = vars_ordered),
      label = sprintf("%.2f", r)
    )
  
  # Smaller labels and axis text when there are many variables
  many_vars <- length(vars) > 15
  label_size <- if (many_vars) 2.2 else 3
  axis_size <- if (many_vars) 11 else 13
  
  ggplot(cor_df, aes(x = var2, y = var1, fill = r)) +
    geom_tile(color = "white", linewidth = 0.3) +
    geom_text(aes(label = label), size = label_size, color = "grey20") +
    scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B",
      midpoint = 0, limits = c(-1, 1), name = "r"
    ) +
    labs(title = title, x = NULL, y = NULL) +
    theme_sheet() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = axis_size),
      axis.text.y = element_text(size = axis_size),
      panel.grid.major = element_line(colour = "grey80", linewidth = 0.2)
    ) +
    coord_fixed()
}

# Print and return pairs with |r| >= threshold
flag_high_cor <- function(data, vars, family_name, threshold = 0.8) {
  mat <- cor(data[, vars, drop = FALSE], use = "pairwise.complete.obs")
  mat[upper.tri(mat, diag = TRUE)] <- NA
  
  pairs <- as.data.frame(as.table(mat)) |>
    dplyr::filter(!is.na(Freq), abs(Freq) >= threshold) |>
    dplyr::arrange(dplyr::desc(abs(Freq))) |>
    dplyr::rename(var1 = Var1, var2 = Var2, r = Freq)
  
  if (nrow(pairs) > 0) {
    cat(family_name, "- pairs with |r| >=", threshold, ":\n")
    print(pairs, row.names = FALSE)
    cat("\n")
  } else {
    cat(family_name, "- no pairs with |r| >=", threshold, "\n\n")
  }
  invisible(pairs)
}

# VIF_j = 1 / (1 - R^2_j), where R^2_j comes from regressing x_j on all other x's
compute_vif <- function(data, vars) {
  col_vars <- sapply(data[, vars, drop = FALSE], var, na.rm = TRUE)
  zero_var <- names(which(col_vars == 0))
  if (length(zero_var) > 0) {
    cat("Dropping zero-variance columns:", paste(zero_var, collapse = ", "), "\n")
    vars <- setdiff(vars, zero_var)
  }
  
  # Regress each variable on all others and convert R^2 to VIF
  vif_vals <- sapply(vars, \(y_var) {
    form <- reformulate(setdiff(vars, y_var), response = y_var)
    r2 <- summary(lm(form, data = data[, vars, drop = FALSE]))$r.squared
    1 / (1 - r2)
  })
  vif_vals
}

# Turn a named VIF vector into a sorted data frame
vif_to_df <- function(vif_values) {
  tibble::tibble(variable = names(vif_values), vif = as.numeric(vif_values)) |>
    dplyr::arrange(dplyr::desc(vif))
}

# VIF bar plot with reference lines at 3 and 5
plot_vif <- function(vif_df, title = NULL) {
  ggplot(vif_df, aes(x = vif, y = reorder(variable, vif))) +
    geom_col(fill = "#5B8DB8", width = 0.7) +
    geom_vline(xintercept = 3, linetype = "dashed", color = "darkblue", linewidth = 0.6) +
    geom_vline(xintercept = 5, linetype = "dashed", color = "#D4A03C", linewidth = 0.6) +
    annotate("text", x = 3, y = 1.5, label = "VIF = 3", color = "darkblue", size = 3, hjust = -0.1) +
    annotate("text", x = 5, y = 1.5, label = "VIF = 5", color = "#D4A03C", size = 3, hjust = -0.1) +
    # Start bars at zero and leave room on the right for the VIF = 5 label
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = title, x = "VIF", y = NULL) +
    theme_sheet() +
    theme(panel.grid.major.y = element_blank())
}

# Drop the highest-VIF variable and recalculate until all VIFs < threshold (Zuur et al. 2010)
vifstep <- function(data, vars, threshold = 3) {
  remaining <- vars
  dropped <- character(0)
  
  # Drop zero-variance columns first
  col_vars <- sapply(data[, remaining, drop = FALSE], var, na.rm = TRUE)
  zero_var <- names(which(col_vars == 0))
  if (length(zero_var) > 0) {
    remaining <- setdiff(remaining, zero_var)
    dropped <- c(dropped, zero_var)
    cat("Dropped (zero variance):", paste(zero_var, collapse = ", "), "\n")
  }
  
  # Remove the worst variable one at a time
  repeat {
    if (length(remaining) <= 2) break
    vifs <- compute_vif(data, remaining)
    if (max(vifs) < threshold) break
    worst <- names(which.max(vifs))
    cat(sprintf("  Dropping %s (VIF = %.1f)\n", worst, max(vifs)))
    dropped <- c(dropped, worst)
    remaining <- setdiff(remaining, worst)
  }
  
  # Report the retained set
  final_vifs <- compute_vif(data, remaining)
  cat("\nRetained variables (", length(remaining), "):\n")
  print(vif_to_df(final_vifs) |> dplyr::mutate(vif = round(vif, 1)), n = Inf)
  cat("\nDropped variables (", length(dropped), "):", paste(dropped, collapse = ", "), "\n\n")
  
  invisible(list(retained = remaining, dropped = dropped, vifs = final_vifs))
}

# Run PCA on complete cases of the given variables
run_pca <- function(data, vars) {
  complete <- data[complete.cases(data[, vars]), vars]
  prcomp(complete, scale. = TRUE)
}

# Scree plot (bars) with cumulative variance (line) for the first n_pc components
plot_scree <- function(pca, title, n_pc = 10) {
  imp <- summary(pca)$importance
  n_pc <- min(n_pc, ncol(imp))
  
  scree_df <- tibble::tibble(
    pc = seq_len(n_pc),
    var_pct = imp[2, seq_len(n_pc)] * 100,
    cum_pct = imp[3, seq_len(n_pc)] * 100
  )
  
  ggplot(scree_df, aes(x = pc, y = var_pct)) +
    geom_col(fill = "#5B8DB8", width = 0.7) +
    geom_line(aes(y = cum_pct), color = "#C25B56", linewidth = 0.8) +
    geom_point(aes(y = cum_pct), color = "#C25B56", size = 2.5) +
    geom_hline(yintercept = c(80, 90), linetype = "dashed", color = "grey60", linewidth = 0.4) +
    scale_x_continuous(breaks = scree_df$pc, labels = paste0("PC", scree_df$pc)) +
    labs(
      title = title, x = NULL,
      y = "Variance explained (%)", caption = "Red line = cumulative %"
    ) +
    theme_sheet()
}

# Print the strongest loadings for the first n_pc components
print_top_loadings <- function(pca, n_pc = 3, n_top = 5) {
  n_pc <- min(n_pc, ncol(pca$rotation))
  for (pc in seq_len(n_pc)) {
    col <- pca$rotation[, pc]
    top <- names(sort(abs(col), decreasing = TRUE))[seq_len(min(n_top, length(col)))]
    cat(sprintf("  PC%d: %s\n", pc, paste(sprintf("%s (%.2f)", top, col[top]), collapse = ", ")))
  }
  cat("\n")
}

# Main function: run all collinearity diagnostics and save Sheets-ready plots
check_covariates <- function(input_file,
                             output_dir,
                             vif_threshold = 3,
                             final_cor_threshold = 0.65,
                             swap_out = c("bio08", "bio02", "log_prop_artificial"),
                             swap_in = c("bio05", "log_imperv_pct"),
                             drop_flags = "pressure_anthropogenic_unknown",
                             dpi = 150) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Shortcut that saves into output_dir at the chosen dpi
  save_out <- function(p, filename, width_px, height_px) {
    save_plot(p, output_dir, filename, width_px, height_px, dpi = dpi)
  }
  
  # Drivers are at catchment level, so keep one row per water body x cycle
  drivers <- arrow::read_parquet(input_file) |>
    dplyr::distinct(euSurfaceWaterBodyCode, cYear, .keep_all = TRUE)
  cat("Driver matrix:", nrow(drivers), "rows (water body x cycle)\n\n")
  
  # Climate, ordered by ecological group: temperature, seasonality, precipitation
  temperature_vars <- c("bio01", "bio02", "bio05", "bio06", "bio07", "bio08", "bio09", "bio10", "bio11")
  seasonality_vars <- c("bio03", "bio04", "log_bio15")
  precipitation_vars <- c("log_bio12", "log_bio13", "log_bio14", "log_bio16", "sqrt_bio17", "sqrt_bio18", "log_bio19")
  climate_vars <- c(temperature_vars, seasonality_vars, precipitation_vars)
  
  # Humidity deficits are log(100 - x): higher = drier
  humidity_vars <- c(
    "hu_annual_mean", "hu_wettest_qtr",
    "log_hu_deficit_min_month", "log_hu_deficit_max_month",
    "log_hu_deficit_warmest_qtr", "log_hu_deficit_coldest_qtr",
    "log_hu_deficit_driest_qtr", "log_hu_range", "log_hu_seasonality"
  )
  nutrient_vars <- c("n_surplus_w", "p_surplus_w")
  land_vars <- c("log_imperv_pct", "log_prop_artificial", "prop_agricultural", "prop_forest", "log_prop_wetlands")
  barrier_vars <- "amber_n_total_log"
  pressure_vars <- grep("^pressure_", names(drivers), value = TRUE)
  
  # bio07 = bio05 - bio06 exactly, so it is shown in heatmaps but excluded from VIF
  cross_family_order <- c(climate_vars, humidity_vars, nutrient_vars, land_vars, barrier_vars)
  all_continuous <- setdiff(cross_family_order, "bio07")
  
  cat("Continuous drivers:", length(all_continuous), "\n")
  cat("Binary drivers:", length(pressure_vars), "\n")
  cat("Total drivers:", length(c(all_continuous, pressure_vars)), "\n\n")
  
  # Within-family correlation heatmaps
  cat("=== Within-family correlations ===\n\n")
  plots <- list()
  plots$cor_climate <- plot_cor_heatmap(drivers, climate_vars, "Climate (bio01-bio19, transformed)", order = climate_vars)
  save_out(plots$cor_climate, "cor_climate.png", 1400, 1200)
  
  plots$cor_humidity <- plot_cor_heatmap(drivers, humidity_vars, "Humidity (transformed)")
  save_out(plots$cor_humidity, "cor_humidity.png", 1000, 900)
  
  plots$cor_land <- plot_cor_heatmap(drivers, land_vars, "Land cover and imperviousness")
  save_out(plots$cor_land, "cor_land.png", 800, 700)
  
  # Flag highly correlated pairs within each family
  high_cor <- list(
    climate = flag_high_cor(drivers, climate_vars, "Climate"),
    humidity = flag_high_cor(drivers, humidity_vars, "Humidity"),
    nutrients = flag_high_cor(drivers, nutrient_vars, "Nutrients"),
    land = flag_high_cor(drivers, land_vars, "Land cover and imperviousness")
  )
  
  # Cross-family heatmap (diagnostic, includes bio07)
  cat("=== Cross-family correlations ===\n\n")
  plots$cor_cross_family <- plot_cor_heatmap(
    drivers, cross_family_order, "All continuous drivers (transformed)", order = cross_family_order
  )
  save_out(plots$cor_cross_family, "cor_cross_family.png", 2000, 1800)
  
  # Strongest climate correlation for each humidity variable
  cross_mat <- cor(drivers[, climate_vars], drivers[, humidity_vars], use = "pairwise.complete.obs")
  cat("Climate x Humidity cross-correlations (max |r| per humidity var):\n")
  print(round(apply(abs(cross_mat), 2, max), 2))
  cat("\n")
  
  # Phi coefficients (Pearson r between 0/1 flags)
  cat("=== Binary predictor collinearity (phi coefficients) ===\n\n")
  plots$cor_binary_phi <- plot_cor_heatmap(drivers, pressure_vars, "Phi coefficients - pressure flags")
  save_out(plots$cor_binary_phi, "cor_binary_phi.png", 1200, 1000)
  high_cor$pressure <- flag_high_cor(drivers, pressure_vars, "Pressure flags", threshold = 0.3)
  
  # VIF of all continuous candidates (can fail if any are perfectly collinear)
  cat("=== VIF analysis (all continuous drivers) ===\n\n")
  tryCatch({
    vif_all_df <- vif_to_df(compute_vif(drivers, all_continuous))
    cat("VIF values (sorted, showing VIF > 3):\n")
    print(dplyr::filter(vif_all_df, vif > 3), n = Inf)
    cat("\nVIF > 10 = severe collinearity, VIF > 5 = moderate, VIF > 3 = concern\n\n")
    plots$vif_all <- plot_vif(vif_all_df, "VIF - all continuous drivers")
    save_out(plots$vif_all, "vif_all_continuous.png", 1000, 1400)
  }, error = \(e) cat("VIF computation failed (likely perfect collinearity):\n", e$message, "\n"))
  
  # Iterative VIF elimination
  cat("=== Iterative VIF elimination (threshold =", vif_threshold, ") ===\n\n")
  vif_result <- vifstep(drivers, all_continuous, threshold = vif_threshold)
  retained_vif <- vif_result$retained
  
  # Ecological swaps on top of the VIF result
  final_continuous <- retained_vif |>
    setdiff(swap_out) |>
    union(swap_in)
  
  # Compare VIF-only selection against each swap and the alternatives considered
  swap_scenarios <- list(
    "vif only" = retained_vif,
    "bio05 for bio08" = c(setdiff(retained_vif, "bio08"), "bio05"),
    "imperv for artificial" = c(setdiff(retained_vif, "log_prop_artificial"), "log_imperv_pct"),
    "forest for agri" = c(setdiff(retained_vif, "prop_agricultural"), "prop_forest"),
    "final set" = final_continuous,
    "final set + N surplus" = c(final_continuous, "n_surplus_w"),
    "final set, forest instead of agri" = c(setdiff(final_continuous, "prop_agricultural"), "prop_forest")
  )
  
  scenario_summary <- purrr::imap(swap_scenarios, \(vars, name) {
    vifs <- compute_vif(drivers, vars)
    tibble::tibble(
      scenario = name,
      n_vars = length(vars),
      max_vif = round(max(vifs), 2),
      highest = names(which.max(vifs))
    )
  }) |>
    purrr::list_rbind()
  
  cat("=== Swap scenarios (max VIF per set) ===\n")
  print(scenario_summary, n = Inf, width = Inf)
  cat("\n")
  
  # VIF of the final continuous set, grouped by family
  vif_final_df <- vif_to_df(compute_vif(drivers, final_continuous)) |>
    dplyr::mutate(
      family = dplyr::case_when(
        grepl("bio", variable) ~ "climate",
        grepl("hu_", variable) ~ "humidity",
        grepl("prop_|imperv", variable) ~ "land_cover",
        grepl("amber", variable) ~ "barriers",
        grepl("surplus", variable) ~ "nutrients",
        .default = "other"
      )
    )
  
  if (max(vif_final_df$vif) >= vif_threshold) {
    cat("Note: VIF >=", vif_threshold, "in the final set (accepted if < 5 and all pairs |r| <", final_cor_threshold, "):\n")
    print(dplyr::filter(vif_final_df, vif >= vif_threshold), n = Inf)
    cat("\n")
  }
  
  plots$vif_final <- plot_vif(vif_final_df, "VIF - final drivers (VIF + ecological swaps)")
  save_out(plots$vif_final, "vif_final.png", 1000, 700)
  
  print(vif_final_df |> dplyr::mutate(vif = round(vif, 2)) |> dplyr::arrange(family, dplyr::desc(vif)), n = Inf)
  
  # VIF of the final set together with the retained pressure flags
  pressure_final <- setdiff(pressure_vars, drop_flags)
  cat("\n=== VIF: final continuous set + pressure flags ===\n")
  vif_flags_df <- vif_to_df(compute_vif(drivers, c(final_continuous, pressure_final)))
  print(vif_flags_df |> dplyr::mutate(vif = round(vif, 2)), n = Inf)
  cat("\n")
  
  plots$vif_flags <- plot_vif(vif_flags_df, "VIF - final drivers + pressure flags")
  save_out(plots$vif_flags, "vif_final_with_flags.png", 1000, 900)
  
  # Correlation matrix of the final set
  plots$cor_final <- plot_cor_heatmap(
    drivers, final_continuous, "Final drivers after VIF and ecological swaps", order = cross_family_order
  )
  save_out(plots$cor_final, "cor_final.png", 1000, 900)
  high_cor$final <- flag_high_cor(drivers, final_continuous, "Final set", threshold = final_cor_threshold)
  
  # PCA within the climate family
  cat("=== PCA: Climate variables (transformed) ===\n\n")
  pca_climate <- run_pca(drivers, climate_vars)
  cat("Cumulative variance explained:\n")
  print(round(summary(pca_climate)$importance[3, 1:min(10, ncol(pca_climate$rotation))], 3))
  plots$scree_climate <- plot_scree(pca_climate, "Climate PCA - scree plot")
  save_out(plots$scree_climate, "pca_scree_climate.png", 1000, 600)
  cat("\nClimate PC loadings (top contributors per PC):\n")
  print_top_loadings(pca_climate, n_top = 5)
  
  # PCA within the humidity family
  cat("=== PCA: Humidity variables (transformed) ===\n\n")
  pca_humidity <- run_pca(drivers, humidity_vars)
  cat("Cumulative variance explained:\n")
  print(round(summary(pca_humidity)$importance[3, ], 3))
  plots$scree_humidity <- plot_scree(pca_humidity, "Humidity PCA - scree plot")
  save_out(plots$scree_humidity, "pca_scree_humidity.png", 1000, 600)
  cat("\nHumidity PC loadings (top contributors per PC):\n")
  print_top_loadings(pca_humidity, n_top = 4)
  
  # Combined climate + humidity PCA to check whether humidity adds information
  cat("=== PCA: Climate + Humidity combined ===\n\n")
  pca_combined <- run_pca(drivers, setdiff(c(climate_vars, humidity_vars), "bio07"))
  cat("Cumulative variance explained (first 10 PCs):\n")
  print(round(summary(pca_combined)$importance[3, 1:min(10, ncol(pca_combined$rotation))], 3))
  cat("\n")
  
  invisible(list(
    drivers = drivers,
    high_cor = high_cor,
    vif_result = vif_result,
    scenario_summary = scenario_summary,
    final_continuous = final_continuous,
    pressure_final = pressure_final,
    vif_final = vif_final_df,
    vif_with_flags = vif_flags_df,
    pca = list(climate = pca_climate, humidity = pca_humidity, combined = pca_combined),
    plots = plots
  ))
}
