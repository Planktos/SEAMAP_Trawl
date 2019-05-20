#AUTHOR : Aidyn Creson
#DATE: May 2019

#create time plot for whole gulf
library(ggplot2)
library (tidyverse)
library(ggThemeAssist)
library(data.table)
library(plyr)
library(gtable)

# geom_ribbon -------------------------------------------------------------
#brainstorms
#possible geom ribbon fix (years)
stdv1 = summary(log(yr_stats$pop_delta_sd_no_m3)+1)
wgp = ggplot(yr_stats, aes(x= year, y= log(pop_delta_mean_no_m3))) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(x= "Year", y=expression(paste("Density ", (kg/m^3)))) +
  geom_ribbon(aes(ymin=yr_stats$pop_delta_mean_no_m3-stdv1, ymax=yr_stats$pop_delta_mean_no_m3+stdv1), fill = "palegreen1" , alpha=.5) +
  #scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
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










# time_series -------------------------------------------------------------
#WHOLE GULF TIME SERIES
yr_stats$pop_delta_mean_no_m3 = ifelse(yr_stats$year == 1985, NA, yr_stats$pop_delta_mean_no_m3)
wgp = ggplot(subset(yr_stats, yr_stats$year >= 1984), aes(x= year, y= pop_delta_mean_no_m3)) +
  geom_point(aes(colour="Whole Gulf")) +
  geom_line(aes(colour="Whole Gulf")) +
  theme_classic() +
  labs(x= "Year", y=expression(paste(" Taxa Density ", (kg/m^3)))) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12)) +
  scale_x_continuous(breaks = seq(1984,2018,2)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  scale_color_manual(values=c("black"))
plot(wgp)

#Diff_by_state time series
states = rbind(fl, la, tx)
states$pop_delta_mean_no_m3 = ifelse(states$year == 1985, NA, states$pop_delta_mean_no_m3)
states$subregion_alongshore = factor(states$subregion_alongshore, levels = c("1_Tex", "2_Lou", "3_Fla"))
stpl = ggplot(subset(states, states$year >=1984), aes(x= year, y = pop_delta_mean_no_m3, colour=subregion_alongshore)) +
  geom_point(na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  theme_classic() +
  labs(x= "Year", y=expression(paste(" Taxa Density ", (kg/m^3)))) +
  facet_grid(rows = vars(subregion_alongshore)) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14)) +
  scale_x_continuous(breaks = seq(1984,2018,2)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  theme(strip.background = element_blank(),strip.text.y = element_blank()) +
  scale_color_manual(breaks = c("1_Tex", "2_Lou", "3_Fla"), values=c("firebrick", "chocolate1", "blue"))
plot(stpl)


#facet fix
  #establish wgp dataframe
year <- c(yr_stats$year)
subregion_alongshore <- c("Whole Gulf")
pop_delta_mean_no_m3 <- c(yr_stats$pop_delta_mean_no_m3)
df <-  data.frame(year, subregion_alongshore, pop_delta_mean_no_m3)
  #establish stpl dataframe
states = rbind(fl, la, tx)
states$pop_delta_mean_no_m3 = ifelse(states$year == 1985, NA, states$pop_delta_mean_no_m3)
states$subregion_alongshore = factor(states$subregion_alongshore, levels = c("1_Tex", "2_Lou", "3_Fla"))
gulfts <- ggplot() +
  # wgp
  geom_point(data=subset(df, df$year >= 1984), aes(x= year, y= pop_delta_mean_no_m3, colour=subregion_alongshore), na.rm=TRUE) +
  geom_line(data=subset(df, df$year >= 1984), aes(x= year, y= pop_delta_mean_no_m3, colour=subregion_alongshore), na.rm=TRUE) +
  # stpl
  geom_point(data=subset(states, states$year >=1984), aes(x= year, y = pop_delta_mean_no_m3, colour=subregion_alongshore), na.rm=TRUE) +
  geom_line(data=subset(states, states$year >=1984), aes(x= year, y = pop_delta_mean_no_m3, colour=subregion_alongshore), na.rm=TRUE) +
  theme_classic() +
  labs(x= "Year", y=expression(paste(" Taxa Density ", (kg/m^3)))) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14)) +
  scale_x_continuous(breaks = seq(1984,2018,2)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  theme(strip.background = element_blank(),strip.text.y = element_blank()) +
  scale_color_manual(breaks = c("1_Tex", "2_Lou", "3_Fla", "Whole Gulf"), values=c("firebrick", "chocolate1", "blue", "black")) +
  #facet
  facet_grid(rows=vars(subregion_alongshore)) +
  ggtitle("Aurelia in the Gulf of Mexico") + theme(plot.title = element_text(hjust = 0.65,
    vjust = 1.2))
plot(gulfts)



# FL plots ----------------------------------------------------------------

#FL inshore
flin =
  ggplot(subset(fl.insh, fl.insh$year >=1984), aes(x=year, y=pop_delta_mean_no_m3, colour="Inshore", fl.insh, fl.insh$year >= 1984)) +
  geom_point(na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  theme_classic() +
  labs(x= "Year", y=expression(paste(" Taxa Density ", (kg/m^3)))) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_x_continuous(breaks = seq(1984,2018,2)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5))
plot(flin)



#FL shelf
dhself = fl.shelf$subregion_depth = factor(fl.shelf$subregion_depth, levels = c("2_shelf"))
fl.shelf$pop_delta_mean_no_m3 = ifelse(fl.shelf$year == 1985, NA, fl.shelf$pop_delta_mean_no_m3)
flsh = ggplot(subset(fl.shelf, fl.shelf$year >= 1984), aes(x= year, y= pop_delta_mean_no_m3)) +
  geom_point(aes(colour="Shelf")) +
  geom_line(aes(colour="Shelf")) +
  theme_classic() +
  labs(x= "Year", y=expression(paste(" Taxa Density ", (kg/m^3)))) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  scale_x_continuous(breaks = seq(1984,2018,2)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  scale_color_manual(values=c("black"))
plot(flsh)


#comparing FL inshore and shelf
flcp <- ggplot() +
  # flin
  geom_point(data=subset(fl.insh, fl.insh$year >=1984), aes(x=year, y=pop_delta_mean_no_m3, na.rm=TRUE,colour="Inshore")) +
  geom_line(data=subset(fl.insh, fl.insh$year >=1984), aes(x=year, y=pop_delta_mean_no_m3, na.rm=TRUE,colour="Inshore")) +
  # flsh
  geom_point(data=subset(fl.shelf, fl.shelf$year >=1984), aes(x=year, y=pop_delta_mean_no_m3, na.rm=TRUE,colour="Shelf")) +
  geom_line(data=subset(fl.shelf, fl.shelf$year >=1984), aes(x=year, y=pop_delta_mean_no_m3, na.rm=TRUE,colour="Shelf")) +
  theme_classic() +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14)) +
  scale_x_continuous(breaks = seq(1984,2018,2)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  labs(x= "Year", y=expression(paste(" Taxa Density ", (kg/m^3)))) +
  scale_color_manual(name="Region", values = c(Shelf="coral3", Inshore="darkseagreen4"))
plot(flcp)
