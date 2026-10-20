# Which columns appear in which SWB tables (thesis Fig. S1 for the raw tables, Fig. S2 after stripping and cleaning)
# Writes a wide table with one row per table, one column per variable (TRUE if present),
# with variables shared by every table first

# Presence of each column across the given tables
column_presence <- function(parquet_root, tables, out_path) {
  # Column names of each table, read from the parquet schema only
  presence <- tables |>
    purrr::map(\(tbl) tibble::tibble(
      table = tbl,
      column = names(arrow::open_dataset(file.path(parquet_root, tbl)))
    )) |>
    purrr::list_rbind()

  # Order columns by how many tables they appear in, then alphabetically
  column_order <- presence |>
    dplyr::count(column, name = "n_tables") |>
    dplyr::arrange(dplyr::desc(n_tables), column) |>
    dplyr::pull(column)

  wide <- presence |>
    dplyr::mutate(present = TRUE, column = factor(column, levels = column_order)) |>
    tidyr::pivot_wider(names_from = column, values_from = present, values_fill = FALSE, names_sort = TRUE)

  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(wide, out_path)
  message(glue::glue("{length(tables)} tables, {length(column_order)} columns, {sum(table(presence$column) == length(tables))} shared by all -> {out_path}"))
  invisible(wide)
}
