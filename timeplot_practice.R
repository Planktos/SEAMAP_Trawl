#create time plot for whole gulf
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






#brainstorms
#possible geom ribbon fix (years)
stdv1 = summary(log10(yr_stats$pop_delta_sd_no_m3)+1)
wgp = ggplot(yr_stats, aes(x= year, y= pop_delta_mean_no_m3)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(x= "Year", y=expression(paste("Density ", (kg/m^3)))) +
  geom_ribbon(aes(ymin=yr_stats$pop_delta_mean_no_m3-stdv1, ymax=yr_stats$pop_delta_mean_no_m3+stdv1), fill = "palegreen1" , alpha=.5) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_x_continuous(breaks = c(1985,1990,1995,2000,2005,2010,2015))
plot(wgp)


#error bars for whole gulf
stdv1 = summary(log10(yr_stats$pop_delta_sd_no_m3)+1)
wgp = ggplot(yr_stats, aes(x= year, y= pop_delta_mean_no_m3)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=yr_stats$pop_delta_mean_no_m3-stdv1, ymax=yr_stats$pop_delta_mean_no_m3+stdv1), colour="black", width=.1) +
  theme_classic() +
  labs(x= "Year", y=expression(paste("Density ", (kg/m^3)))) +
  #geom_ribbon(aes(ymin=yr_stats$pop_delta_mean_no_m3-yr_stats$pop_delta_sd_no_m3, ymax=yr_stats$pop_delta_mean_no_m3+yr_stats$pop_delta_sd_no_m3))
  scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_x_continuous(breaks = c(1985,1990,1995,2000,2005,2010,2015))
plot(wgp)

#time series for FL
flsd = summary(log10(fl$pop_delta_sd_no_m3)+1)
flp = ggplot(fl, aes(x= year, y= pop_delta_mean_no_m3)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=fl$pop_delta_mean_no_m3-flsd, ymax=fl$pop_delta_mean_no_m3+flsd), fill = "red" , alpha=.3) +
  theme_classic() +
  labs(x= "Year", y=expression(paste("Density ", (kg/m^3)))) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_x_continuous(breaks = c(1985,1990,1995,2000,2005,2010,2015))
plot(flp)

#time series for LA
lasd = summary(log10(la$pop_delta_sd_no_m3)+1)
lap = ggplot(la, aes(x= year, y= pop_delta_mean_no_m3)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=la$pop_delta_mean_no_m3-lasd, ymax=la$pop_delta_mean_no_m3+lasd), fill = "green" , alpha=.3) +
  theme_classic() +
  labs(x= "Year", y=expression(paste("Density ", (kg/m^3)))) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_x_continuous(breaks = c(1985,1990,1995,2000,2005,2010,2015))
plot(lap)

#time series for TX
txsd = summary(log10(tx$pop_delta_sd_no_m3)+1)
txp = ggplot(tx, aes(x= year, y= pop_delta_mean_no_m3)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=tx$pop_delta_mean_no_m3-txsd, ymax=la$pop_delta_mean_no_m3+txsd), fill = "blue" , alpha=.3) +
  theme_classic() +
  labs(x= "Year", y=expression(paste("Density ", (kg/m^3)))) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_x_continuous(breaks = c(1985,1990,1995,2000,2005,2010,2015))
plot(txp)









