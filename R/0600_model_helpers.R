# Helpers shared by the model assessment (06) and figure (07) pipelines

# Saved fit files in a results folder, one per BQE
list_fit_files <- function(results_dir) {
  list.files(path.expand(results_dir), pattern = "^fit_.*\\.RData$", full.names = TRUE)
}

# BQE name from a fit file name
bqe_from_path <- function(path) {
  sub("^fit_(.*)\\.RData$", "\\1", basename(path))
}

# Load the clmm object from a saved file, whatever name it was saved under
load_fit <- function(path) {
  # The ordinal namespace is needed for the clmm methods (logLik, model.matrix, ...)
  loadNamespace("ordinal")
  e <- new.env()
  load(path, envir = e)
  objs <- mget(ls(e), envir = e)
  fit <- purrr::detect(objs, \(x) inherits(x, "clmm"))
  if (is.null(fit)) {
    fit <- purrr::detect(purrr::list_flatten(objs), \(x) inherits(x, "clmm"))
  }
  if (is.null(fit)) stop("No clmm object found in ", path)
  fit
}

# Class probabilities under the probit link: P(Y = k) = Phi((theta_k - eta) / s) - Phi((theta_k-1 - eta) / s)
class_probs <- function(eta, theta, s = 1) {
  cum <- pnorm(outer(-eta, theta, "+") / s)
  cbind(cum, 1) - cbind(0, cum)
}

# Load every saved BQE result (the bqe_result list written by fit_stage1), named by BQE
load_bqe_results <- function(results_dir) {
  loadNamespace("ordinal")
  fit_files <- list_fit_files(results_dir)
  purrr::map(fit_files, \(f) {
    e <- new.env()
    load(f, envir = e)
    e$bqe_result
  }) |>
    purrr::set_names(bqe_from_path(fit_files)) |>
    purrr::keep(\(r) !is.null(r$fit))
}
