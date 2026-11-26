# Presentation figures: one example water body shown in its catchment, basin and Europe
# Run from the project root after run_03_joins.R; figures go to plots/presentation

# Load packages and functions
library(ggplot2)
source("presentation/theme_presentation.R")
source("presentation/wb_context_maps.R")
source("presentation/wb_cycle_bars.R")
source("presentation/wb_unit_maps.R")

# River water bodies in the Tagus basin with fish assessed, ranked by number of cycles
candidates <- find_basin_wbs("tag|tajo|tejo", qe_code = "QE1-4 - Fish")
print(candidates, n = 10)

# Map the top candidate; pick another row of candidates to change the example
example <- candidates[1, ]
wb_maps <- plot_wb_context(
  wb_code = example$euSurfaceWaterBodyCode,
  title = example$surfaceWaterBodyName,
  out_dir = "plots/presentation"
)

# River water bodies in the models per cycle, split by how many cycles each appears in
cycle_bars <- plot_wb_cycle_bars(out_file = "plots/presentation/wb_cycle_bars.png")


# The same water body inside its reported River Basin District, with the CCM2 basin dashed on top
ctx <- read_wb_context(example$euSurfaceWaterBodyCode)
rbd <- read_wfd_unit(example$euSurfaceWaterBodyCode, cycle = 2022, level = "rbd")
rbd_map <- plot_wb_in_unit(
  ctx, rbd,
  title = example$surfaceWaterBodyName,
  subtitle = glue::glue("River Basin District {rbd$unit_code[1]} ({stringr::str_to_title(rbd$unit_name[1])})"),
  caption = "Shaded: reported RBD   Dashed: CCM2 river basin",
  out_file = glue::glue("plots/presentation/{example$euSurfaceWaterBodyCode}_5_rbd.png")
)

# The same water body inside its biogeographical region, framed on the RBD and basin
biogeo <- read_biogeo_unit(ctx)
biogeo_map <- plot_wb_in_unit(
  ctx, biogeo,
  title = example$surfaceWaterBodyName,
  subtitle = glue::glue("Biogeographical region ({biogeo$unit_name[1]})"),
  view = c(sf::st_geometry(rbd), sf::st_geometry(ctx$basin)),
  out_file = glue::glue("plots/presentation/{example$euSurfaceWaterBodyCode}_6_biogeo.png")
)
