#AUTHOR : Aidyn Creson
#DATE:  26 April 2019

#load libraries
library(ggplot2)
library (tidyverse)
library(ggThemeAssist)
library(data.table)
library(plyr)
library(gtable)
library(dplyr)

# AURELIA geom_ribbon --------
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

# AURELIA time_series -------------------------------------------------------------
#WHOLE GULF TIME SERIES
yr_stats$pop_delta_mean_no_m3 = ifelse(yr_stats$year == 1985, NA, yr_stats$pop_delta_mean_no_m3)
wgp <- ggplot(subset(yr_stats, yr_stats$year >= 1984), aes(x= year, y= log(pop_delta_mean_no_m3+1))) +
  geom_point(aes(colour="Whole Gulf")) +
  geom_line(aes(colour="Whole Gulf")) +
  theme_classic() +
  labs(x= "Year", y=expression(paste(" Taxa Density ", (kg/m^3)))) +
  scale_y_continuous(breaks = seq(0,4,0.5)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  scale_color_manual(values=c("black")) +
  ggtitle("Aurelia_WholeGulf_1984-2018") +
  theme(plot.title = element_text(size = 16, hjust = 0.5))
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
  scale_color_manual(breaks = c("Whole Gulf", "1_Tex", "2_Lou", "3_Fla"), values=c("firebrick", "chocolate1", "blue", "black")) +
  #facet
  facet_grid(rows=vars(subregion_alongshore)) +
  ggtitle("Aurelia in the Gulf of Mexico") + theme(plot.title = element_text(hjust = 0.65,
    vjust = 1.2))
plot(gulfts)


# AURELIA FL plots ----------------------------------------------------------------
#comparing FL inshore and shelf
fl.shelf$pop_delta_mean_no_m3 = ifelse(fl.shelf$year == 1985, NA, fl.shelf$pop_delta_mean_no_m3)
fl.insh$pop_delta_mean_no_m3 = ifelse(fl.insh$year == 1985, NA, fl.insh$pop_delta_mean_no_m3)
flcp <- ggplot() +
  # flin
  geom_point(data=subset(fl.insh, fl.insh$year >=1984), aes(x=year,y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Inshore")) +
  geom_line(data=subset(fl.insh, fl.insh$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Inshore")) +
  # flsh
  geom_point(data=subset(fl.shelf, fl.shelf$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf")) +
  geom_line(data=subset(fl.shelf, fl.shelf$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf")) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0,4,0.5), limits = c(0,4)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  ggtitle("Aurelia_Florida_1984-2018") +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  labs(x= "Year", y=expression(paste(" logTaxa Density ", (kg/m^3)))) +
  scale_color_manual(name="Region", values = c(Shelf="coral3", Inshore="darkseagreen4")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 16))
file = "AURELIA_FL.png"
png(file=file, width=12,height=9,units="in", res=225)
plot(flcp)
dev.off()


# AURELIA LA plots --------------------------------------------------------
#comparing LA inshore and shelf
la.shelf$pop_delta_mean_no_m3 = ifelse(la.shelf$year == 1985, NA, la.shelf$pop_delta_mean_no_m3)
la.insh$pop_delta_mean_no_m3 = ifelse(la.insh$year == 1985, NA, la.insh$pop_delta_mean_no_m3)
lacp <- ggplot() +
  # lain
  geom_point(data=subset(la.insh, la.insh$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Inshore")) +
  geom_line(data=subset(la.insh, la.insh$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Inshore")) +
  # lash
  geom_point(data=subset(la.shelf, la.shelf$year >=1984), aes(x=year,y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf")) +
  geom_line(data=subset(la.shelf, la.shelf$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf")) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0,4,0.5), limits = c(0,4)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  labs(x= "Year", y=expression(paste(" logTaxa Density ", (kg/m^3)))) +
  scale_color_manual(name="Region", values = c(Shelf="coral3", Inshore="darkseagreen4")) +
  ggtitle("Aurelia_Louisiana_1984-2018")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 16))
file = "AURELIA_LA.png"
png(file=file, width=12,height=9,units="in", res=225)
plot(lacp)
dev.off()




# AURELIA TX plots --------------------------------------------------------
#comparing TX inshore and shelf
tx.shelf$pop_delta_mean_no_m3 = ifelse(tx.shelf$year == 1985, NA, tx.shelf$pop_delta_mean_no_m3)
tx.insh$pop_delta_mean_no_m3 = ifelse(tx.insh$year == 1985, NA, tx.insh$pop_delta_mean_no_m3)
txcp <- ggplot() +
  # txin
  geom_point(data=subset(tx.insh, tx.insh$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Inshore")) +
  geom_line(data=subset(tx.insh, tx.insh$year >=1984), aes(x=year,y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Inshore")) +
  # txsh
  geom_point(data=subset(tx.shelf, tx.shelf$year >=1984), aes(x=year,y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf")) +
  geom_line(data=subset(tx.shelf, tx.shelf$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf")) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0,4,0.5), limits = c(0,4)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  labs(x= "Year", y=expression(paste(" logTaxa Density ", (kg/m^3)))) +
  scale_color_manual(name="Region", values = c(Shelf="coral3", Inshore="darkseagreen4")) +
  theme(plot.title = element_text(size = 16, hjust = 0.5)) +
  ggtitle("Aurelia_Texas_1984-2018")
file = "AURELIA_TX.png"
png(file=file, width=12,height=9,units="in", res=225)
plot(txcp)
dev.off()



# Prep Chrysaora Data Means -----------------------------------------------
Chrys_data_mean = ddply(chrysaora_by_state, .(year), summarize,
                        use_pop_den_mean_no_m2 = mean(use_pop_den_no_m2))

Chrys_tx_data_mean = ddply(chrys_tx, .(year, subregion_depth), summarize,
                           use_pop_den_mean_no_m2 = mean(use_pop_den_no_m2))

Chrys_la_data_mean = ddply(chrys_la, .(year, subregion_depth), summarize,
                           use_pop_den_mean_no_m2 = mean(use_pop_den_no_m2))

Chrys_fl_data_mean = ddply(chrys_fl, .(year, subregion_depth), summarize,
                           use_pop_den_mean_no_m2 = mean(use_pop_den_no_m2))





# AURELIA and CHRYSAORA whole gulf ----------------------------------------

wgp2 = ggplot() +
  geom_point(data=subset(yr_stats, yr_stats$year >= 1984), aes(x=year,y=log(pop_delta_mean_no_m3+1)), colour="chocolate1") +
  geom_line(data=subset(yr_stats, yr_stats$year >= 1984), aes(x=year,y=log(pop_delta_mean_no_m3+1)), colour="chocolate1") +
  geom_point(data=subset(Chrys_data_mean, Chrys_data_mean$year >=1984), aes(x=year,y=log(use_pop_den_mean_no_m2+1)), colour="firebrick") +
  geom_line(data=subset(Chrys_data_mean, Chrys_data_mean$year >=1984), aes(x=year,y=log(use_pop_den_mean_no_m2+1)), colour="firebrick") +
  theme_classic() +
  labs(x= "Year", y=expression(paste(" Taxa Density ", (kg/m^3)))) +
  scale_y_continuous(breaks = seq(0,4,0.5)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  scale_color_manual(values=c("black")) +
  ggtitle("Aurelia_and_Chrysaora_WholeGulf_1984-2018") +
  theme(plot.title = element_text(size = 16, hjust = 0.5))
plot(wgp2)



# CHRYSOARA LA  -----------------------------------------------------------
cla = ggplot(subset(chrys_la,subregion_depth %in% c("1_inshore" , "2_shelf"))) +
  geom_line(data=subset(chrys_la, chrys_la$year >=1984), aes(x=year, y=log(pop_den_no_m3+1), group=subregion_depth, colour=subregion_depth)) +
  geom_point(data=subset(chrys_la, chrys_la$year >=1984), aes(x=year, y=log(pop_den_no_m3+1), group=subregion_depth, colour=subregion_depth)) +
  theme_classic() +
  labs(x= "Year", y=expression(paste(" Taxa Density ", (kg/m^3)))) +
  scale_y_continuous(breaks = seq(0,4,0.5), limits = c(0,4)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  scale_color_manual(breaks = c("1_inshore", "2_shelf", "oceanic"), values=c("firebrick", "chocolate1", "blue")) +
  ggtitle("Chrysaora_Louisiana_1984-2018") +
  theme(plot.title = element_text(size = 16, hjust = 0.5))
plot(cla)


# CHRYSAORA TX ------------------------------------------------------------
ctx = ggplot(subset(chrys_tx,subregion_depth %in% c("1_inshore" , "2_shelf"))) +
  geom_line(data=subset(chrys_tx, chrys_tx$year >=1984), aes(x=year, y=log(pop_den_no_m3+1), group=subregion_depth, colour=subregion_depth)) +
  geom_point(data=subset(chrys_tx, chrys_tx$year >=1984), aes(x=year, y=log(pop_den_no_m3+1), group=subregion_depth, colour=subregion_depth)) +
  theme_classic() +
  labs(x= "Year", y=expression(paste(" Taxa Density ", (kg/m^3)))) +
  scale_y_continuous(breaks = seq(0,4,0.5), limits = c(0,4)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  scale_color_manual(breaks = c("1_inshore", "2_shelf", "oceanic"), values=c("firebrick", "chocolate1", "blue")) +
  ggtitle("Chrysaora_Texas_1984-2018") +
  theme(plot.title = element_text(size = 16, hjust = 0.5))
plot(ctx)









# CHRYSAORA FL ------------------------------------------------------------
cfl = ggplot(subset(chrys_fl,subregion_depth %in% c("1_inshore" , "2_shelf"))) +
  geom_line(data=subset(chrys_fl, chrys_fl$year >=1984), aes(x=year, y=log(pop_den_no_m3+1), group=subregion_depth, colour=subregion_depth)) +
  geom_point(data=subset(chrys_fl, chrys_fl$year >=1984), aes(x=year, y=log(pop_den_no_m3+1), group=subregion_depth, colour=subregion_depth)) +
  theme_classic() +
  labs(x= "Year", y=expression(paste(" Taxa Density ", (kg/m^3)))) +
  scale_y_continuous(breaks = seq(0,4,0.5), limits = c(0,4)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  scale_color_manual(breaks = c("1_inshore", "2_shelf", "oceanic"), values=c("firebrick", "chocolate1", "blue")) +
  ggtitle("Chrysaora_Florida_1984-2018") +
  theme(plot.title = element_text(size = 16, hjust = 0.5))
plot(cfl)
















