# for additional information on a function type: ?function_name

# ensure required packages are loaded
library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)
library(jaggernaut)
library(mwst2)

# load required data (from mwstdatr)
data(spawners)
data(gsi)

# plot spawner data
plot_spawners(spawners)

# print JAGS model code
cat(spawners_model_code())

# perform spawner analysis and print summary
analysis <- analyse_spawners(spawners)
summary(analysis)

# save analysis object
saveRDS(analysis, "results/spawners-analysis.rds")

#' save spawner traceplots
pdf("results/spawners-traceplots.pdf")
plot(analysis)
dev.off()

# plot residuals
pdf("results/spawners-residuals.pdf", width = 4, height = 4, units = "in", res = getOption("res", 150))
plot_residuals(analysis)
dev.off()

# plot number of spawners with credible intervals
pdf("results/spawners-fit.pdf", width = 3, height = 3, units = "in", res = getOption("res", 150))
add_fit_lines(plot_spawners(spawners), predict_spawners(analysis))
dev.off()

# predict timing, set year to be 2011 and plot
timing_spawners <- predict_timing(analysis)
timing_spawners$Year <- 2011
plot_timing(timing_spawners)

# plot Gonadosomatic Index
plot_gsi(gsi) + facet_grid(Sex ~ Year, scales = "free_y")

# plot JAGS model code
cat(gsi_model_code())

# analyse GSI data for 2010
analysis10 <- analyse_gsi(filter(gsi, Year == 2010))
summary(analysis10)

# save analysis object
saveRDS(analysis10, "results/gsi-analysis10.rds")

#' save spawner traceplots
pdf("results/gsi-traceplots10.pdf")
plot(analysis10)
dev.off()

# plot residuals
pdf("results/gsi-residuals10.pdf", width = 4, height = 4, units = "in", res = getOption("res", 150))
plot_residuals(analysis10) + facet_wrap(~Sex)
dev.off()

# analyse GSI data for 2011
analysis11 <- analyse_gsi(filter(gsi, Year == 2011))
summary(analysis11)

# save analysis object
saveRDS(analysis11, "results/gsi-analysis11.rds")

#' save spawner traceplots
pdf("results/gsi-traceplots11.pdf")
plot(analysis11)
dev.off()

# plot residuals
pdf("results/gsi-residuals11.pdf", width = 4, height = 4, units = "in", res = getOption("res", 150))
plot_residuals(analysis11) + facet_wrap(~Sex)
dev.off()

# predict GSI values for each year and combine
fit10 <- predict_gsi(analysis10)
fit11 <- predict_gsi(analysis11)
fit10$Year <- 2010
fit11$Year <- 2011
fit <- rbind(fit10, fit11)

# plot GSI data with predictions and credible intervals and save
pdf("results/gsi-fit.pdf", width = 4, height = 4, units = "in", res = getOption("res", 150))
(plot_gsi(gsi) + facet_grid(Sex ~ Year, scales = "free_y")) %>% add_fit_lines(fit)
dev.off()

# predict spawn timing by year and combine
timing10 <- predict_timing(analysis10)
timing11 <- predict_timing(analysis11)
timing10$Year <- 2010
timing11$Year <- 2011
timing_gsi <- rbind(timing10, timing11)

timing_gsi$Method <- "Gonadosomatic Index"
timing_spawners$Method <- "Spawner Count"

# plot spawn timing by method and save
timing <- rbind(timing_gsi, timing_spawners)
pdf("results/timing.pdf", width = 4, height = 3, units = "in", res = getOption("res", 150))
plot_timing(timing) + facet_wrap(~Method)
dev.off()

saveRDS(timing, "results/timing.rds")
