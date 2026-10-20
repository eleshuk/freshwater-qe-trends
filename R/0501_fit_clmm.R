# M4 stage 1: per-BQE ordinal CLMMs
# Final covariate set: 11 transformed continuous drivers + 7 pressure flags,
# main effects only, cycle * region, random intercepts for water body and basin,
# plus country when fit_stage1(country = TRUE)
# BQEs with a saved fit_<bqe>.RData are skipped unless refit is TRUE
# Fits are written to <model_dir>/M4/m4_stage1_results_rivers/RW (about 45 minutes for all five on a MacBook Air)

# Diagonal of the inverse of the random-effects precision matrix, solved in column chunks.
# clmm computes this for condVar with solve(rho$L), which builds the full inverse; with a
# country RE that inverse is dense (about q^2 values, ~30 GB for Benthic invertebrates)
diag_inv_chunked <- function(L, chunk = 500) {
  q <- nrow(L)
  out <- numeric(q)
  for (start in seq(1, q, by = chunk)) {
    idx <- start:min(start + chunk - 1, q)
    rhs <- matrix(0, q, length(idx))
    rhs[cbind(idx, seq_along(idx))] <- 1
    sol <- as.matrix(Matrix::solve(L, rhs, system = "A"))
    out[idx] <- sol[cbind(idx, seq_along(idx))]
  }
  out
}

# Patch ordinal's internal fitter to use the chunked version; estimates, SEs and condVar are unchanged
patch_clmm_condvar <- function() {
  fit_env <- utils::getFromNamespace("clmm.fit.env", "ordinal")
  body_txt <- deparse(body(fit_env), width.cutoff = 500L)

  # Skip if this session has already patched the fitter
  if (any(grepl("diag_inv_chunked(rho$L)", body_txt, fixed = TRUE))) {
    message("clmm.fit.env already patched, skipping")
    return(invisible(FALSE))
  }

  # Stop if the original call isn't found exactly once, e.g. after an ordinal update
  target <- "diag(solve(rho$L))"
  if (sum(grepl(target, body_txt, fixed = TRUE)) != 1) {
    stop("Could not find diag(solve(rho$L)) in ordinal:::clmm.fit.env; check the ordinal version")
  }

  # Swap in the chunked version and give it access to diag_inv_chunked
  body_txt <- sub(target, "diag_inv_chunked(rho$L)", body_txt, fixed = TRUE)
  body(fit_env) <- parse(text = paste(body_txt, collapse = "\n"))[[1]]
  patch_env <- new.env(parent = environment(fit_env))
  patch_env$diag_inv_chunked <- diag_inv_chunked
  environment(fit_env) <- patch_env
  utils::assignInNamespace("clmm.fit.env", fit_env, ns = "ordinal")
  invisible(TRUE)
}

# Water category codes mapped to the names used in the data file and results folder
category_names <- c(RW = "rivers", LW = "lakes")

# River BQEs modelled in the thesis (phytoplankton is mainly assessed in lakes, so it is not modelled for rivers)
river_bqes <- c(
  "QE1-2 - Other aquatic flora", "QE1-2-3 - Macrophytes",
  "QE1-2-4 - Phytobenthos", "QE1-3 - Benthic invertebrates", "QE1-4 - Fish"
)

# Optimizer settings by run label; gradTol is the tolerance of the inner random-effect solve,
# iter.max and eval.max limit the outer nlminb optimizer
opt_settings_all <- list(
  main = list(grad_tol = 1e-3, iter_max = 500, eval_max = 700),
  sens = list(grad_tol = 1e-4, iter_max = 1000, eval_max = 2000)
)

# Final covariates (transformed columns from R/0402_transform_rivers_data.R)
continuous_vars <- c(
  "bio05", "bio04", "log_bio15", "sqrt_bio18",
  "log_hu_deficit_coldest_qtr", "log_hu_deficit_driest_qtr",
  "p_surplus_w", "log_imperv_pct", "prop_agricultural", "log_prop_wetlands",
  "amber_n_total_log"
)

# Pressure flags; pressure_anthropogenic_unknown is an adjustment covariate, not interpreted as a driver
pressure_vars <- c(
  "pressure_diffuse_sources", "pressure_point_sources",
  "pressure_hydromorphology", "pressure_abstraction",
  "pressure_introduced_species_litter", "pressure_anthropogenic_other",
  "pressure_anthropogenic_unknown"
)

all_model_vars <- c(continuous_vars, pressure_vars)

# Columns the model uses, read alone to save memory
keep_cols <- c(
  "surfaceWaterBodyCategory", "countryCode", "qeCode", "cYear",
  "euSurfaceWaterBodyCode", "ccm2_basin", "biogeographical_region",
  "qeStatusOrPotentialValue", "qeStatus", all_model_vars
)

# Pull one RE variance by name, NA if absent
get_re_var <- function(re, name) {
  if (!is.null(re) && name %in% names(re)) re[[name]][1] else NA_real_
}

# Fit one clmm per BQE; country toggles the (1 | country) random intercept
fit_stage1 <- function(model_dir = "data/model", country = TRUE, category = "RW", run_label = "main",
                       bqe_only = NULL, refit = FALSE) {
  # Use the memory-saving condVar calculation in ordinal
  patch_clmm_condvar()
  
  # Check arguments and look up settings for this run
  if (!category %in% names(category_names)) {
    stop("Unknown category: ", category, " (expected one of ", paste(names(category_names), collapse = ", "), ")")
  }
  if (!run_label %in% names(opt_settings_all)) {
    stop("Unknown run_label: ", run_label, " (expected one of ", paste(names(opt_settings_all), collapse = ", "), ")")
  }
  opt_settings <- opt_settings_all[[run_label]]
  category_name <- category_names[[category]]
  data_path <- file.path(model_dir, paste0(category_name, "_model_df_transformed.parquet"))
  
  # Output folder: category, then the run label unless main, then _no_country without the country RE
  run_folder <- paste0(
    category,
    if (run_label != "main") paste0("_", run_label),
    if (!country) "_no_country"
  )
  out_dir <- file.path(model_dir, "M4", paste0("m4_stage1_results_", category_name), run_folder)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Random-effect grouping factors used in the formula, linear predictor and diagnostics
  re_groups <- c("waterBody", "basin", if (country) "country")
  re_formula <- paste0("(1 | ", re_groups, ")", collapse = " + ")
  
  # Append to the existing log so reruns keep earlier BQE output; close the sink even if the run errors
  sink(file.path(out_dir, "m4_fitting_log.txt"), append = TRUE, split = TRUE)
  on.exit(sink(), add = TRUE)
  cat("\n=== Run started:", format(Sys.time(), "%Y-%m-%d %H:%M"), "===\n")
  
  set.seed(42)
  
  cat("ordinal version:", as.character(packageVersion("ordinal")), "\n")
  cat("R version:", R.version.string, "\n")
  cat("Platform:", .Platform$OS.type, Sys.info()["machine"], "\n")
  cat("Water category:", category, "\n")
  cat("Random effects:", re_formula, "\n")
  cat("Output folder:", out_dir, "\n")
  cat("Refit existing BQEs:", refit, "\n")
  cat("Run:", run_label, "| gradTol:", opt_settings$grad_tol,
      "| iter.max:", opt_settings$iter_max, "| eval.max:", opt_settings$eval_max, "\n\n")
  
  # Load transformed data; category and CY/MT filters are defensive (the transform script already drops CY/MT)
  df <- arrow::read_parquet(data_path, col_select = dplyr::any_of(keep_cols)) |>
    dplyr::filter(
      surfaceWaterBodyCategory == category,
      !countryCode %in% c("CY", "MT")
    )
  cat("Dataset:", nrow(df), "rows x", ncol(df), "cols\n")
  cat("BQEs:", paste(sort(unique(df$qeCode)), collapse = ", "), "\n")
  cat("Cycles:", paste(sort(unique(df$cYear)), collapse = ", "), "\n")
  cat("Countries:", dplyr::n_distinct(df$countryCode), "\n\n")
  
  # Observations per BQE
  bqe_counts <- df |>
    dplyr::count(qeCode, name = "n_obs") |>
    dplyr::arrange(dplyr::desc(n_obs))
  print(as.data.frame(bqe_counts))
  cat("\n")
  
  # Stop if any covariate is missing, since the set is final and silent dropping would change the model
  missing_vars <- setdiff(all_model_vars, names(df))
  if (length(missing_vars) > 0) {
    stop("Variables not in data: ", paste(missing_vars, collapse = ", "))
  }
  
  # Encode response as an ordered factor (1 = High ... 5 = Bad)
  if ("qeStatusOrPotentialValue" %in% names(df)) {
    df$status_int <- as.integer(as.character(df$qeStatusOrPotentialValue))
  } else if ("qeStatus" %in% names(df)) {
    lvl_map <- c("High" = 1L, "Good" = 2L, "Moderate" = 3L, "Poor" = 4L, "Bad" = 5L)
    df$status_int <- lvl_map[as.character(df$qeStatus)]
  } else {
    stop("No status column found (expected qeStatusOrPotentialValue or qeStatus)")
  }
  df$status_ord <- factor(df$status_int, levels = 1:5, ordered = TRUE)
  
  cat("Response distribution:\n")
  print(table(df$status_ord, useNA = "ifany"))
  cat("\n")
  
  # Centre and divide by 2 SD on this category's rows so continuous effects compare with binary flags
  scale_info <- list()
  for (v in continuous_vars) {
    mu <- mean(df[[v]], na.rm = TRUE)
    sd_v <- sd(df[[v]], na.rm = TRUE)
    if (sd_v == 0) sd_v <- 1
    df[[v]] <- (df[[v]] - mu) / (2 * sd_v)
    scale_info[[v]] <- list(center = mu, scale = 2 * sd_v)
  }
  saveRDS(scale_info, file = file.path(out_dir, "scale_info.rds"))
  cat("Scaled", length(scale_info), "continuous variables (centred, divided by 2 SD)\n\n")
  
  # Biogeographical region column (NULL if absent)
  biogeo_col <- if ("biogeographical_region" %in% names(df)) "biogeographical_region" else NULL
  
  bqe_list <- sort(unique(df$qeCode))
  if (!is.null(bqe_only)) bqe_list <- intersect(bqe_list, bqe_only)
  results <- list()
  
  # Fit one model per BQE
  for (bqe in bqe_list) {
    
    cat("\n########################################\n")
    cat("BQE:", bqe, "(", category, ")\n")
    cat("########################################\n\n")
    
    # Skip BQEs with a saved fit, but still add their row to the summary
    fit_file <- file.path(out_dir, paste0("fit_", bqe, ".RData"))
    if (!refit && file.exists(fit_file)) {
      cat("Found", basename(fit_file), "- skipping refit\n")
      load(fit_file)
      re_saved <- bqe_result$re_variance
      results[[bqe]] <- data.frame(
        bqe = bqe, status = bqe_result$status, n_obs = bqe_result$n_obs,
        n_covariates = bqe_result$n_covariates,
        n_waterBodies = bqe_result$n_waterBodies, n_basins = bqe_result$n_basins,
        n_countries = bqe_result$n_countries,
        AIC = AIC(bqe_result$fit), logLik = as.numeric(logLik(bqe_result$fit)),
        var_waterBody = get_re_var(re_saved, "waterBody"),
        var_basin = get_re_var(re_saved, "basin"),
        var_country = get_re_var(re_saved, "country"),
        time_min = bqe_result$time_min
      )
      rm(bqe_result, re_saved)
      gc()
      next
    }
    
    df_bqe <- df |> dplyr::filter(qeCode == bqe)
    cat("Raw observations:", nrow(df_bqe), "\n")
    print(table(df_bqe$status_ord, useNA = "ifany"))
    
    bqe_model_vars <- all_model_vars
    
    # Complete-case filter on covariates
    na_before <- nrow(df_bqe)
    df_bqe <- df_bqe |>
      dplyr::filter(dplyr::if_all(dplyr::all_of(bqe_model_vars), \(x) !is.na(x)))
    cat("After complete-case filter:", nrow(df_bqe), "rows (dropped",
        na_before - nrow(df_bqe), ")\n")
    
    # Flag sparse ordinal categories
    status_tab <- table(df_bqe$status_ord)
    if (any(status_tab < 10)) {
      cat("WARNING: sparse ordinal categories:",
          paste(names(status_tab[status_tab < 10]), "=", status_tab[status_tab < 10],
                collapse = ", "), "\n")
    }
    
    # Grouping factors with unused levels dropped; country is kept for site IDs even without the country RE
    df_bqe$waterBody <- factor(df_bqe$euSurfaceWaterBodyCode)
    df_bqe$basin <- factor(as.character(df_bqe$ccm2_basin))
    df_bqe$country <- factor(df_bqe$countryCode)
    df_bqe$cycle <- factor(df_bqe$cYear)
    
    cat("Water body levels:", nlevels(df_bqe$waterBody), "\n")
    cat("Basin levels:", nlevels(df_bqe$basin), "\n")
    cat("Country levels:", nlevels(df_bqe$country), "\n")
    if (country && nlevels(df_bqe$country) < 5) {
      cat("WARNING: fewer than 5 countries, country variance will be poorly estimated\n")
    }
    
    if (!is.null(biogeo_col)) {
      df_bqe$region <- factor(df_bqe[[biogeo_col]])
      cat("Biogeographical regions (", nlevels(df_bqe$region), "levels):",
          paste(levels(df_bqe$region), collapse = ", "), "\n")
    }
    
    # Drop covariates with zero variance for this BQE (e.g. a flag never set)
    drop_vars <- bqe_model_vars[vapply(bqe_model_vars, \(v) {
      v_var <- var(df_bqe[[v]], na.rm = TRUE)
      is.na(v_var) || v_var == 0
    }, logical(1))]
    if (length(drop_vars) > 0) {
      cat("Dropping zero-variance columns:", paste(drop_vars, collapse = ", "), "\n")
      bqe_model_vars <- setdiff(bqe_model_vars, drop_vars)
    }
    
    # Main effects only for covariates, plus cycle, region and cycle x region
    fixed_terms <- c(bqe_model_vars, "cycle")
    if (!is.null(biogeo_col)) {
      fixed_terms <- c(fixed_terms, "region", "cycle:region")
    }
    
    # Formula with the random intercepts chosen by the country argument
    formula_str <- paste("status_ord ~", paste(fixed_terms, collapse = " + "), "+", re_formula)
    model_formula <- as.formula(formula_str)
    cat("\nFormula:", formula_str, "\n")
    cat("Observations:", nrow(df_bqe), "\n\n")
    
    # Fit clmm with the optimizer settings for this run (see opt_settings_all)
    cat("Fitting clmm (probit,", paste(re_groups, collapse = " + "), "RE)...\n")
    t0 <- proc.time()
    
    fit <- tryCatch(
      ordinal::clmm(
        model_formula,
        data = df_bqe,
        link = "probit",
        Hess = TRUE,
        model = TRUE,
        nAGQ = 1,
        control = ordinal::clmm.control(
          maxIter = 1000,
          gradTol = opt_settings$grad_tol,
          maxLineIter = 100,
          trace = 1,
          iter.max = opt_settings$iter_max,
          eval.max = opt_settings$eval_max
        )
      ),
      error = function(e) {
        cat("ERROR:", conditionMessage(e), "\n")
        NULL
      }
    )
    
    elapsed <- (proc.time() - t0)[3]
    cat("Time:", round(elapsed / 60, 1), "minutes\n")
    
    # Summary row for a failed fit (not saved to disk, so it is refitted next run)
    if (is.null(fit)) {
      cat("Model failed for", bqe, "-- skipping.\n")
      results[[bqe]] <- data.frame(
        bqe = bqe, status = "failed", n_obs = nrow(df_bqe),
        n_covariates = length(bqe_model_vars),
        n_waterBodies = nlevels(df_bqe$waterBody), n_basins = nlevels(df_bqe$basin),
        n_countries = nlevels(df_bqe$country), AIC = NA_real_, logLik = NA_real_,
        var_waterBody = NA_real_, var_basin = NA_real_, var_country = NA_real_,
        time_min = round(elapsed / 60, 1)
      )
      rm(df_bqe)
      gc()
      next
    }
    
    # Convergence checks from the nlminb result
    converged <- isTRUE(fit$optRes$convergence == 0)
    practically_converged <- converged || isTRUE(fit$optRes$convergence == 1)
    cat("Log-likelihood:", as.numeric(logLik(fit)), "\n")
    cat("AIC:", AIC(fit), "\n")
    cat("Converged:", converged, "\n")
    if (!converged) cat("Optimizer message:", fit$optRes$message, "\n")
    grad <- fit$optRes$gradient
    if (is.numeric(grad)) cat("Max absolute gradient:", max(abs(grad)), "\n")
    cat("Practically converged:", practically_converged, "\n")
    
    re_var <- ordinal::VarCorr(fit)
    cat("\nRandom effect variances:\n")
    print(re_var)
    cat("\n")
    
    # Coefficient table
    coef_tab <- tryCatch({
      beta <- summary(fit)$coefficients
      data.frame(
        variable = rownames(beta),
        estimate = beta[, "Estimate"],
        se = beta[, "Std. Error"],
        z_value = beta[, "z value"],
        p_value = beta[, "Pr(>|z|)"],
        row.names = NULL
      )
    }, error = function(e) {
      cat("WARNING: could not extract coefficient table:", conditionMessage(e), "\n")
      NULL
    })
    
    if (!is.null(coef_tab)) {
      cat("Top 10 coefficients by |z|:\n")
      coef_tab |>
        dplyr::arrange(dplyr::desc(abs(z_value))) |>
        head(10) |>
        print(digits = 3)
      cat("\n")
    }
    
    thresholds <- fit$Theta
    cat("Ordinal thresholds:", paste(round(thresholds, 3), collapse = ", "), "\n\n")
    
    ranef_vals <- tryCatch(ordinal::ranef(fit), error = function(e) NULL)
    
    # Linear predictor eta = X beta + conditional modes of every RE in the model.
    # A missing RE would silently empty eta (x + NULL is numeric(0)), so length and NAs are checked
    lin_pred <- tryCatch({
      mm <- model.matrix(fit)
      if (is.list(mm)) mm <- mm$X
      xb <- as.numeric(mm[, names(fit$beta), drop = FALSE] %*% fit$beta)
      re_eta <- re_groups |>
        purrr::map(\(grp) ranef_vals[[grp]][as.character(df_bqe[[grp]]), 1]) |>
        purrr::reduce(`+`, .init = 0)
      eta <- xb + re_eta
      if (length(eta) != nrow(df_bqe) || anyNA(eta)) {
        stop("eta has length ", length(eta), " and ", sum(is.na(eta)), " NAs; expected ", nrow(df_bqe), " values")
      }
      cat("Linear predictor range:", round(min(eta), 3), "to", round(max(eta), 3), "\n")
      eta
    }, error = function(e) {
      cat("WARNING: linear predictor extraction failed:", conditionMessage(e), "\n")
      NULL
    })
    
    # Diagnostic plots: eta by status, RE QQ plots, observed vs predicted classes
    tryCatch({
      pdf(file.path(out_dir, paste0("diagnostics_", bqe, ".pdf")), width = 12, height = 8)
      par(mfrow = c(2, 3))
      
      if (!is.null(lin_pred)) {
        boxplot(lin_pred ~ df_bqe$status_ord,
                xlab = "Observed status", ylab = "Linear predictor",
                main = paste(bqe, "- Linear predictor by status"),
                col = "lightblue", outline = FALSE)
      }
      
      for (re_name in re_groups) {
        if (!is.null(ranef_vals[[re_name]])) {
          re_vec <- ranef_vals[[re_name]][, 1]
          qqnorm(re_vec, main = paste(bqe, "-", re_name, "RE QQ"),
                 pch = 16, cex = ifelse(re_name == "country", 0.8, 0.3))
          qqline(re_vec, col = "red")
        }
      }
      
      pred_prob <- tryCatch(predict(fit, newdata = df_bqe, type = "prob")$fit,
                            error = function(e) NULL)
      if (!is.null(pred_prob)) {
        pred_class <- apply(pred_prob, 1, which.max)
        obs_tab <- prop.table(table(df_bqe$status_ord))
        pred_tab <- prop.table(table(factor(pred_class, levels = 1:5)))
        barplot(rbind(obs_tab, pred_tab), beside = TRUE,
                col = c("steelblue", "coral"), names.arg = 1:5,
                main = paste(bqe, "- Observed vs Predicted"),
                xlab = "Status", ylab = "Proportion")
        legend("topright", c("Observed", "Predicted"),
               fill = c("steelblue", "coral"), cex = 0.8)
      }
      
      dev.off()
      cat("Diagnostic plots saved.\n")
    }, error = function(e) {
      cat("WARNING: could not generate diagnostic plots:", conditionMessage(e), "\n")
      tryCatch(dev.off(), error = function(e2) NULL)
    })
    
    site_ids <- data.frame(
      euSurfaceWaterBodyCode = as.character(df_bqe$euSurfaceWaterBodyCode),
      countryCode = as.character(df_bqe$country),
      cYear = df_bqe$cYear
    )
    
    # Full result for this BQE, saved to disk only; re_groups records which REs the fit used
    bqe_result <- list(
      fit = fit,
      status = ifelse(practically_converged, "converged", "not_converged"),
      category = category,
      re_groups = re_groups,
      time_min = round(elapsed / 60, 1),
      n_obs = nrow(df_bqe),
      n_covariates = length(bqe_model_vars),
      n_waterBodies = nlevels(df_bqe$waterBody),
      n_basins = nlevels(df_bqe$basin),
      n_countries = nlevels(df_bqe$country),
      coef_table = coef_tab,
      linear_predictors = lin_pred,
      thresholds = thresholds,
      re_variance = re_var,
      covariate_names = bqe_model_vars,
      site_ids = site_ids
    )
    
    # Save the model, coefficients and linear predictors for this BQE
    save(bqe_result, file = fit_file)
    if (!is.null(coef_tab)) {
      write.csv(coef_tab, file.path(out_dir, paste0("coefficients_", bqe, ".csv")),
                row.names = FALSE)
    }
    if (!is.null(lin_pred)) {
      write.csv(dplyr::mutate(site_ids, linear_predictor = lin_pred),
                file.path(out_dir, paste0("linear_predictors_", bqe, ".csv")),
                row.names = FALSE)
    }
    cat("Model saved:", basename(fit_file), "\n")
    
    # Keep only a summary row in memory; the full fit is on disk
    results[[bqe]] <- data.frame(
      bqe = bqe, status = bqe_result$status, n_obs = nrow(df_bqe),
      n_covariates = length(bqe_model_vars),
      n_waterBodies = nlevels(df_bqe$waterBody), n_basins = nlevels(df_bqe$basin),
      n_countries = nlevels(df_bqe$country), AIC = AIC(fit),
      logLik = as.numeric(logLik(fit)),
      var_waterBody = get_re_var(re_var, "waterBody"),
      var_basin = get_re_var(re_var, "basin"),
      var_country = get_re_var(re_var, "country"),
      time_min = round(elapsed / 60, 1)
    )
    
    # Free per-BQE objects before the next fit
    rm(list = intersect(
      c("df_bqe", "fit", "lin_pred", "coef_tab", "ranef_vals", "bqe_result",
        "site_ids", "pred_prob", "re_var"),
      ls()
    ))
    gc()
  }
  
  # Summary table across BQEs (skipped BQEs are read back from their saved fits)
  cat("\n========================================\n")
  cat("STAGE 1 SUMMARY (", category, ")\n")
  cat("========================================\n")
  cat("Method: clmm, probit link, Laplace (nAGQ=1)\n")
  cat("Random effects:", re_formula, "\n")
  cat("Convergence: maxIter=1000, gradTol=", opt_settings$grad_tol, ", maxLineIter=100, iter.max=",
      opt_settings$iter_max, ", eval.max=", opt_settings$eval_max, "\n\n", sep = "")
  
  summary_df <- dplyr::bind_rows(results)
  print(summary_df, digits = 4)
  write.csv(summary_df, file.path(out_dir, "stage1_summary.csv"), row.names = FALSE)
  
  cat("\nAll results saved to", out_dir, "\n")
  cat("Total fitting time (this and earlier runs):", sum(summary_df$time_min, na.rm = TRUE), "minutes\n")
  
  invisible(summary_df)
}
