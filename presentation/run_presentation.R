# Presentation figures: one example water body shown in its catchment, basin and Europe
# Run from the project root after run_03_joins.R; figures go to plots/presentation

# Load packages and functions
library(ggplot2)
source("presentation/theme_presentation.R")
source("presentation/wb_context_maps.R")
source("presentation/wb_cycle_bars.R")

# River water bodies in the Tagus basin with fish assessed, ranked by number of cycles
candidates <- find_basin_wbs("tag|tajo|tejo", qe_code = "QE1-4 - Fish")
print(candidates, n = 20)

# Map the top candidate; pick another row of candidates to change the example
example <- candidates[1, ]
wb_maps <- plot_wb_context(
  wb_code = example$euSurfaceWaterBodyCode,
  title = example$surfaceWaterBodyName,
  out_dir = "plots/presentation"
)

# River water bodies in the models per cycle, split by how many cycles each appears in
cycle_bars <- plot_wb_cycle_bars(out_file = "plots/presentation/wb_cycle_bars.png")

