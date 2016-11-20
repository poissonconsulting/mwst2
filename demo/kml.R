# for additional information on a function type: ?function_name

# ensure required packages are loaded
library(magrittr)
library(plotKML)
library(sp)
library(mwst2)

#' create directory to store results
dir.create("results", showWarnings = FALSE, recursive = TRUE)

# load required data sets (from mwstdatr)
data(mats)

# save map as kml file
mats %<>% spTransform(CRS("+proj=longlat +datum=WGS84"))
mats[["Site"]] %<>% as.character()
mats@data$Colour <- "Colour"

plotKML(mats, "results/map.kml", colour = "Colour", points_name = mats[["Site"]])
