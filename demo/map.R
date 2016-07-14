# for additional information on a function type: ?function_name

# ensure required packages are loaded
library(ggplot2)
library(magrittr)
library(mwst2)

#' create directory to store results
dir.create("results", showWarnings = FALSE, recursive = TRUE)

# load required data sets (from mwstdatr)
data(ldr)
data(lardeau)
data(duncan)
data(kootenay)
data(mats)

# crop spatial extent
ldr %<>% crop()
lardeau %<>% crop()
duncan %<>% crop()
kootenay %<>% crop()

png("results/ldr.png", width = 3, height = 5, units = "in", res = getOption("res", 150))

#' plot a map of the study area
map(ldr, colour = "grey70", fill = "grey70") +
  add_layer(kootenay, colour = "grey70", fill = "grey70") +
  add_layer(duncan, colour = "grey70", fill = "grey70") +
  add_layer(lardeau, colour = "grey70", fill = "grey70") +
  add_layer(mats) +
  add_layer(mats, label = "RiverKm", hjust = -0.25, size = 2.5) +
  theme(panel.grid = element_line())
dev.off()
