# Colours, font and base theme shared by the presentation figures, matched to the defense slides; ggplot2 must be loaded

# Colours and font of the defense slides
pres_cols <- c(
  background = "#FAF9F7",
  dark_green = "#1B4D20",
  green = "#37633B",
  pale_green = "#DCE3DA",
  tan = "#BFA97D",
  dark_tan = "#8C7748",
  land = "#E9E6DF",
  land_border = "#C9C3B6"
)
pres_font <- "Montserrat"

# Base theme: slide background, Montserrat text in the slide greens
theme_presentation <- function(base_size = 14, font = pres_font) {
  theme_minimal(base_size = base_size, base_family = font) +
    theme(
      plot.background = element_rect(fill = pres_cols[["background"]], colour = NA),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", colour = pres_cols[["dark_green"]]),
      plot.subtitle = element_text(colour = pres_cols[["green"]]),
      axis.text = element_text(colour = pres_cols[["dark_green"]]),
      axis.title = element_text(colour = pres_cols[["green"]]),
      legend.text = element_text(colour = pres_cols[["dark_green"]]),
      legend.title = element_text(colour = pres_cols[["green"]]),
      plot.margin = margin(12, 12, 12, 12)
    )
}
