# create time plot for whole gulf

library(ggplot2)
library (tidyverse)
library(ggThemeAssist)
library(data.table)
library(plyr)
library (dplyr)

wgp = ggplot(yr_stats, aes(x= year, y= pop_delta_mean_no_m3)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(x= "Year", y=expression(paste("Density ", (kg/m^3))))
  #geom_ribbon(aes(ymin=yr_stats$pop_delta_mean_no_m3-yr_stats$pop_delta_sd_no_m3, ymax=yr_stats$pop_delta_mean_no_m3+yr_stats$pop_delta_sd_no_m3))

plot(wgp)
