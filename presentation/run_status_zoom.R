# Zoomed observed vs modelled status figures for the slides; run from the project root
# Figures go to plots/presentation

# Load packages and functions
library(ggplot2)
source("R/theme_thesis.R")
source("R/0600_model_helpers.R")
source("R/0702_plot_temporal_region.R")
source("presentation/status_zoom.R")

# Fitted models and the observed and modelled shares for every zoom case
results <- load_bqe_results("data/model")
zoom_data <- build_zoom_data(results)

# Alpine and Boreal fish
save_status_zoom(zoom_data, zoom_cases[1:2, ], "plots/presentation/status_zoom_alpine_boreal_fish.png")

# Cases where observed and modelled trends diverge
save_status_zoom(zoom_data, zoom_cases[3:4, ], "plots/presentation/status_zoom_divergent_cases.png")

# All four cases on one slide
save_status_zoom(zoom_data, zoom_cases, "plots/presentation/status_zoom_all.png")
