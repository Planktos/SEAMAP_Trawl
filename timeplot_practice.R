# create time plot for whole gulf

library(ggplot2)
library (tidyverse)
library(ggThemeAssist)
library(data.table)
library(plyr)
library (dplyr)

#WHOLE GULF TIME SERIES
wgp = ggplot(yr_stats, aes(x= year, y= pop_delta_mean_no_m3)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(x= "Year", y=expression(paste("Density ", (kg/m^3)))) +
  #geom_ribbon(aes(ymin=yr_stats$pop_delta_mean_no_m3-yr_stats$pop_delta_sd_no_m3, ymax=yr_stats$pop_delta_mean_no_m3+yr_stats$pop_delta_sd_no_m3))
  scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_x_continuous(breaks = c(1985,1990,1995,2000,2005,2010,2015))

plot(wgp)

#Diff_by_state time series

states = rbind(fl, la, tx)

stpl = ggplot(data=states, aes(x= year, y = pop_delta_mean_no_m3, colour=subregion_alongshore)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(x= "Year", y=expression(paste("Density ", (kg/m^3)))) +
  #geom_ribbon(aes(ymin=yr_stats$pop_delta_mean_no_m3-yr_stats$pop_delta_sd_no_m3, ymax=yr_stats$pop_delta_mean_no_m3+yr_stats$pop_delta_sd_no_m3))
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14)) +
  scale_x_continuous(breaks = c(1985,1990,1995,2000,2005,2010,2015))

plot(stpl)



