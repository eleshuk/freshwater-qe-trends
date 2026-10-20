# Shared theme and save helper for figures going into the thesis document; ggplot2 must be loaded

# Text width of a Google Docs page with 1 inch margins
doc_width <- 6.5

# Base theme for all thesis figures
theme_thesis <- function(base_size = 10) {
  theme_bw(base_size = base_size) +
    theme(
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.text = element_text(size = rel(0.8)),
      plot.caption = element_text(hjust = 0, size = rel(0.8))
    )
}

# Save at page width so text keeps its size in the doc; titles are dropped because the doc caption carries them
save_doc_plot <- function(plot, file, height, width = doc_width, keep_titles = FALSE) {
  if (!keep_titles) plot <- plot + labs(title = NULL, subtitle = NULL)
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  ggsave(file, plot, width = width, height = height, dpi = 300)
}
