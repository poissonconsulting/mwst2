# for additional information on a function type: ?function_name

# ensure required packages are loaded
library(ggplot2)
library(magrittr)
library(mwst2)

# create results/plots, results/pdfs and results/rds directories to store results
create_dirs()

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

# plot map
map(ldr, colour = "grey70", fill = "grey70") +
  add_layer(kootenay, colour = "grey70", fill = "grey70") +
  add_layer(duncan, colour = "grey70", fill = "grey70") +
  add_layer(lardeau, colour = "grey70", fill = "grey70") +
  add_layer(mats) +
  add_layer(mats, label = "RiverKm", hjust = -0.25, size = 2.5)

# save plot as eps in results/plots
save_plot("ldr", height = 4.5)
