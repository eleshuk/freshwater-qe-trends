# Scrape WISE WFD table and column descriptions from https://discodata.eea.europa.eu/md

# Write a data frame to both parquet and excel
write_outputs <- function(df, file_name, out_dir) {
  arrow::write_parquet(df, file.path(out_dir, paste0(file_name, ".parquet")))
  openxlsx::write.xlsx(df, file.path(out_dir, paste0(file_name, ".xlsx")))
}

# Scrape table and column metadata and write full and theme-filtered versions
scrape_wise_wfd_md <- function(root_path, theme_filter = "SWB") {
  out_dir <- file.path(root_path, "metadata")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Request the metadata once and reuse it for tables and columns
  md_raw <- httr2::request("https://discodata.eea.europa.eu/md") |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  
  # Find the WISE_WFD entry and its latest schema by name
  wise_schema <- md_raw |>
    purrr::keep(\(x) x[["name"]] == "WISE_WFD") |>
    purrr::pluck(1, "Schemas") |>
    purrr::keep(\(s) s[["name"]] == "latest") |>
    purrr::pluck(1)
  
  # Extract table descriptions and add theme from the table name prefix
  table_md <- wise_schema[["Tables"]] |>
    purrr::map(\(tbl) tibble::tibble(
      id = tbl[["id"]],
      name = tbl[["name"]],
      description = tbl[["description"]]
    )) |>
    purrr::list_rbind() |>
    dplyr::mutate(theme = sub("_.*", "", name))
  
  # Extract column descriptions nested within each table
  column_md <- wise_schema[["Tables"]] |>
    purrr::map(\(tbl) tbl[["Columns"]] |>
                 purrr::map(\(col) tibble::tibble(
                   id = col[["id"]],
                   table = col[["table"]],
                   column = col[["name"]],
                   description = col[["description"]]
                 )) |>
                 purrr::list_rbind()
    ) |>
    purrr::list_rbind() |>
    dplyr::mutate(theme = sub("_.*", "", table))
  
  # Keep only the chosen theme, all columns retained so repeats stay visible
  theme_table_md <- table_md |>
    dplyr::filter(theme == theme_filter) |>
    dplyr::rename(table = name) |>
    dplyr::select(-id) |>
    dplyr::relocate(theme, .before = table)
  
  theme_column_md <- column_md |>
    dplyr::filter(theme == theme_filter) |>
    dplyr::select(-id) |>
    dplyr::relocate(theme, .before = table)
  
  # Write all four outputs
  prefix <- tolower(theme_filter)
  write_outputs(table_md, "wise_wfd_table_discodata_md", out_dir)
  write_outputs(theme_table_md, paste0(prefix, "_table_discodata_md"), out_dir)
  write_outputs(column_md, "wise_discodata_md", out_dir)
  write_outputs(theme_column_md, paste0(prefix, "_discodata_md"), out_dir)
  
  invisible(list(
    tables = table_md,
    columns = column_md,
    theme_tables = theme_table_md,
    theme_columns = theme_column_md
  ))
}
