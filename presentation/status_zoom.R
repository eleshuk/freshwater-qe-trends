# Zoomed versions of thesis Fig. 7 and 8 for the slides: observed and modelled status shares for a few region x BQE cases
# Needs R/0600_model_helpers.R, R/0702_plot_temporal_region.R and R/theme_thesis.R sourced and ggplot2 loaded

# Cases shown on the slides, as region and BQE label pairs
zoom_cases <- tibble::tribble(
  ~region_label, ~bqe_label,
  "Alpine", "Fish",
  "Boreal", "Fish",
  "Mediterranean", "Fish",
  "Pannonian", "Benthic Invertebrates"
)

# Observed records from the model input parquet when it is there, otherwise from the records stored in the fits
read_zoom_observed <- function(results, data_path = "data/model/rivers_model_df_transformed.parquet") {
  if (file.exists(data_path)) return(read_observed_status(data_path, names(results)))
  message("No ", data_path, "; using the records stored in the fits")
  purrr::imap(results, function(r, bqe) {
    r$fit$model |>
      dplyr::transmute(
        bqe_code = stringr::str_extract(bqe, "QE[\\d-]+"),
        bqe_label = unname(temporal_bqe_labels[bqe]),
        status_int = as.character(status_ord),
        status_label = factor(status_labels[status_int], levels = c("Bad", "Poor", "Moderate", "Good", "High")),
        cycle_label = as.character(cycle),
        region_label = stringr::str_to_title(as.character(region))
      )
  }) |>
    purrr::list_rbind()
}

# Observed shares and modelled class probabilities for the chosen cases, stacked in one data frame
build_zoom_data <- function(results, cases = zoom_cases, table_dir = "data/model/tables",
                            data_path = "data/model/rivers_model_df_transformed.parquet") {
  observed <- read_zoom_observed(results, data_path)
  cells <- count_cells(observed)

  obs <- observed |>
    dplyr::semi_join(cases, by = c("region_label", "bqe_label")) |>
    dplyr::count(bqe_label, cycle_label, region_label, status_label) |>
    dplyr::mutate(value = n / sum(n), cell_n = sum(n), .by = c(bqe_label, cycle_label, region_label)) |>
    dplyr::mutate(source = "Observed")

  # Predicted classes from the 07 pipeline table if it exists, otherwise predicted here (point estimates only)
  pred_file <- file.path(table_dir, "cycle_x_region_status_observed.csv")
  pred <- if (file.exists(pred_file)) {
    read.csv(pred_file) |>
      dplyr::mutate(cycle_label = as.character(cycle_label),
                    status_label = factor(status_label, levels = levels(obs$status_label)))
  } else {
    bqes <- names(temporal_bqe_labels)[temporal_bqe_labels %in% cases$bqe_label]
    purrr::imap(results[bqes], \(r, bqe) predict_bqe(r$fit, bqe, n_sim = 10)$classes) |>
      purrr::list_rbind() |>
      dplyr::mutate(
        cycle_label = cycle,
        region_label = stringr::str_to_title(region),
        status_label = factor(status_labels[status_int], levels = levels(obs$status_label))
      ) |>
      dplyr::inner_join(cells, by = c("bqe_code", "cycle_label", "region_label"))
  }
  pred <- pred |>
    dplyr::semi_join(cases, by = c("region_label", "bqe_label")) |>
    dplyr::transmute(bqe_label, cycle_label, region_label, status_label, value = probs, cell_n, source = "Modelled")

  dplyr::bind_rows(obs, pred) |>
    dplyr::mutate(
      case = factor(paste(region_label, bqe_label, sep = "\n"),
                    levels = paste(cases$region_label, cases$bqe_label, sep = "\n")),
      source = factor(source, levels = c("Observed", "Modelled"))
    )
}

# Stacked status bars faceted by source (rows) and case (columns), sized for a slide
plot_status_zoom <- function(zoom_data, min_n = 30, base_size = 16) {
  zoom_n <- dplyr::distinct(zoom_data, case, source, cycle_label, cell_n)

  ggplot(zoom_data, aes(x = cycle_label, y = value, fill = status_label)) +
    geom_col(aes(alpha = cell_n < min_n), position = "stack", width = 0.7) +
    geom_text(data = zoom_n, aes(x = cycle_label, y = 1.03, label = scales::comma(cell_n)),
              inherit.aes = FALSE, size = base_size / 4, vjust = 0) +
    facet_grid(source ~ case) +
    scale_fill_manual(values = wfd_colours, guide = guide_legend(reverse = TRUE)) +
    scale_alpha_manual(values = c("FALSE" = 1, "TRUE" = 0.3), guide = "none") +
    scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, 0.25),
                       expand = expansion(mult = c(0, 0.12))) +
    labs(x = "Assessment cycle", y = "Share of assessments", fill = "Status") +
    theme_thesis(base_size = base_size) +
    theme(
      axis.text = element_text(size = rel(0.85)),
      # Transparent around the panels so the slide background shows through, white inside them
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

# Save one figure per set of cases; width grows with the number of columns
save_status_zoom <- function(zoom_data, cases, out_file, height = 7) {
  d <- dplyr::semi_join(zoom_data, cases, by = c("region_label", "bqe_label")) |>
    dplyr::mutate(case = droplevels(case))
  p <- plot_status_zoom(d)
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  ggsave(out_file, p, width = 2 + 3.2 * nrow(cases), height = height, dpi = 300, bg = "transparent")
  p
}
