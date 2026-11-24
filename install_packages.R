# Install the R packages the pipeline uses; run once from the repository root
# MASS, Matrix, stats and utils come with R and are not listed

# Packages used by the scripts in R/, extract_drivers/, run_*.R and tests/
packages <- c(
  "arrow", "data.table", "DBI", "dplyr", "exactextractr", "fs", "ggplot2",
  "glue", "httr2", "matrixStats", "openxlsx", "ordinal", "purrr", "readr",
  "readxl", "RSQLite", "scales", "sf", "sfarrow", "snakecase", "stringr",
  "terra", "tibble", "tidyr", "vroom"
)

# Install only the ones that are missing
missing <- packages[!packages %in% rownames(installed.packages())]
if (length(missing) > 0) {
  install.packages(missing)
} else {
  message("All packages are already installed")
}

# Report anything that still fails to load
failed <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(failed) > 0) {
  warning("These packages did not install or load: ", paste(failed, collapse = ", "))
} else {
  message("All ", length(packages), " packages load")
}
