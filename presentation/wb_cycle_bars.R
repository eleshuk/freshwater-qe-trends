# Bar plot of the river water bodies used in the models in each reporting cycle, split by how many cycles
# each water body appears in. The data are the records in data/model/linear_predictors_*.csv, i.e. the
# 243,549 complete river records after removing Cyprus, Malta and missing covariates (Results 3.1, Table 5)
# Uses pres_cols, pres_font and theme_presentation() from presentation/theme_presentation.R

# Counts in thesis Results 3.1 ("Final dataset"), printed next to the rebuilt counts for comparison
thesis_records <- 243549
thesis_totals <- c("2010" = 17446, "2016" = 47905, "2022" = 58469)
thesis_unique <- 71311

# Water bodies per cycle, grouped by the number of cycles each one appears in
count_wb_cycle_presence <- function(df) {
  presence <- df |>
    dplyr::distinct(cYear, euSurfaceWaterBodyCode) |>
    dplyr::mutate(n_cycles = dplyr::n(), .by = euSurfaceWaterBodyCode)

  presence |>
    dplyr::mutate(
      cycle = factor(cYear, levels = c(2010, 2016, 2022), labels = c("2010\n1st cycle", "2016\n2nd cycle", "2022\n3rd cycle")),
      group = factor(
        n_cycles,
        levels = c(1, 2, 3),
        labels = c("In this cycle only", "In two cycles", "In all three cycles")
      )
    ) |>
    dplyr::count(cYear, cycle, group, name = "n")
}

# Read the modelled river records from the linear predictor files written by R/0501_fit_clmm.R
read_modelled_rivers <- function(model_dir = "data/model") {
  list.files(model_dir, pattern = "^linear_predictors_.*\\.csv$", full.names = TRUE) |>
    purrr::map(\(f) readr::read_csv(f, col_select = c("euSurfaceWaterBodyCode", "cYear"), show_col_types = FALSE)) |>
    purrr::list_rbind()
}

# Stacked bars of river water bodies per cycle; returns the plot and, with out_file set, saves it
plot_wb_cycle_bars <- function(model_dir = "data/model", out_file = NULL, width = 10, height = 6, font = pres_font) {
  df <- read_modelled_rivers(model_dir)
  counts <- count_wb_cycle_presence(df)

  # Compare with the thesis text
  cat("Records:", nrow(df), "(thesis:", thesis_records, ")\n")
  cat("Unique water bodies:", dplyr::n_distinct(df$euSurfaceWaterBodyCode), "(thesis:", thesis_unique, ")\n")
  print(
    counts |>
      dplyr::summarise(total = sum(n), .by = cYear) |>
      dplyr::mutate(thesis = thesis_totals[as.character(cYear)])
  )

  plot_wb_cycle_counts(counts, out_file, width, height, font)
}

# Draw the bars from the counts made by count_wb_cycle_presence()
plot_wb_cycle_counts <- function(counts, out_file = NULL, width = 10, height = 6, font = pres_font) {
  # Persistent water bodies at the bottom so the shared core lines up across bars; new ones on top in the accent colour
  group_cols <- c(
    "In all three cycles" = pres_cols[["dark_green"]],
    "In two cycles" = "#7B977C",
    "In this cycle only" = pres_cols[["tan"]]
  )
  label_cols <- c(
    "In all three cycles" = pres_cols[["background"]],
    "In two cycles" = pres_cols[["background"]],
    "In this cycle only" = pres_cols[["dark_green"]]
  )
  counts <- counts |>
    dplyr::mutate(group = factor(group, levels = names(group_cols)))
  totals <- counts |>
    dplyr::summarise(n = sum(n), .by = cycle)

  # Segment labels only where the segment is tall enough to hold one
  min_label <- 0.06 * max(totals$n)

  p <- ggplot(counts, aes(x = cycle, y = n, fill = group)) +
    geom_col(width = 0.65, position = position_stack(reverse = TRUE)) +
    geom_text(
      data = dplyr::filter(counts, n >= min_label),
      aes(label = scales::comma(n), colour = group),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      family = font, size = 4.2, show.legend = FALSE
    ) +
    geom_text(
      data = totals,
      aes(x = cycle, y = n, label = scales::comma(n)),
      inherit.aes = FALSE, vjust = -0.6, family = font, fontface = "bold", size = 5.5,
      colour = pres_cols[["dark_green"]]
    ) +
    scale_fill_manual(values = group_cols, breaks = rev(names(group_cols)), name = NULL) +
    scale_colour_manual(values = label_cols, guide = "none") +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1))) +
    labs(
      title = "River water bodies in the models",
      subtitle = "By reporting cycle",
      x = NULL, y = "River water bodies"
    ) +
    theme_presentation(font = font) +
    theme(
      legend.position = "right",
      axis.text.x = element_text(face = "bold", size = rel(1)),
      axis.line.x = element_line(colour = pres_cols[["green"]])
    )

  # Save at slide-friendly size on the slide background
  if (!is.null(out_file)) {
    dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
    ggsave(out_file, p, width = width, height = height, dpi = 300, bg = pres_cols[["background"]])
  }

  p
}
