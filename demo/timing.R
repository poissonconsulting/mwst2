# for additional information on a function type: ?function_name

# ensure required packages are loaded
library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)
library(jaggernaut)
library(mwst2)

# create results/plots, results/pdfs and results/rds directories to store results
create_dirs()

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

# save analysis object and pdfs of traceplots and residuals
save_rds(analysis, "spawners-analysis")
save_pdf(analysis, "spawners-traceplots")
plot_residuals(analysis)
save_plot("spawners-residuals", width = 4, height = 4)

# predict number of spawners and plot with credible intervals and save
fit <- predict_spawners(analysis)
add_fit_lines(plot_spawners(spawners), fit)
save_plot("spawners-fit")

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

# save analysis object and pdf of traceplots and eps of residuals
save_rds(analysis10, "gsi-analysis10")
save_pdf(analysis10, "gsi-traceplots10")
plot_residuals(analysis10) + facet_wrap(~Sex)
save_plot("gsi-residuals10", width = 4, height = 4)

# analyse GSI data for 2011
analysis11 <- analyse_gsi(filter(gsi, Year == 2011))
summary(analysis11)

# save analysis object and pdf of traceplots and eps of residuals
save_rds(analysis11, "gsi-analysis11")
save_pdf(analysis11, "gsi-traceplots11")
plot_residuals(analysis11) + facet_wrap(~Sex)
save_plot("gsi-residuals11", width = 4, height = 4)

# predict GSI values for each year and combine
fit10 <- predict_gsi(analysis10)
fit11 <- predict_gsi(analysis11)
fit10$Year <- 2010
fit11$Year <- 2011
fit <- rbind(fit10, fit11)

# plot GSI data with predictions and credible intervals and save
(plot_gsi(gsi) + facet_grid(Sex ~ Year, scales = "free_y")) %>% add_fit_lines(fit)
save_plot("gsi-fit", width = 4, height = 4)

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
plot_timing(timing) + facet_wrap(~Method)
save_plot("timing", width = 4)
save_rds(timing)
