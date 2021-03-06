# for additional information on a function type: ?function_name

# ensure required packages are loaded
library(ggplot2)
library(magrittr)
library(dplyr)
library(mwst2)

#' create directory to store results
dir.create("results", showWarnings = FALSE, recursive = TRUE)

# load required data sets (from mwstdatr)
data(ldr)
data(lardeau)
data(duncan)
data(kootenay)
data(mats)
data(transect)

# crop spatial extent
ldr %<>% crop()
lardeau %<>% crop()
duncan %<>% crop()
kootenay %<>% crop()

labels <- data_frame(Label = "Lower Duncan River", Easting = 1644.5, Northing = 618.75)
labels %<>% bind_rows(data_frame(Label = "Duncan Reservoir", Easting = 1644.25, Northing = 623.75))
labels %<>% bind_rows(data_frame(Label = "Kootenay Lake", Easting = 1645.5, Northing = 613.5))
labels$group <- "Label"

pdf("results/ldr.pdf", width = 3, height = 5)

#' plot a map of the study area
map(ldr, colour = "grey70", fill = "grey70") +
  add_layer(kootenay, colour = "grey70", fill = "grey70") +
  add_layer(duncan, colour = "grey70", fill = "grey70") +
  add_layer(lardeau, colour = "grey70", fill = "grey70") +
  add_layer(mats) +
  add_layer(transect, color = "red", shape = 17) +
  add_layer(mats, label = "RiverKm", size = 2.5) +
  geom_text(data = labels, aes(x = Easting, y = Northing, label = Label), size = 2) +
  theme(panel.grid = element_line())
dev.off()
