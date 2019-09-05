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



<<<<<<< HEAD


=======
>>>>>>> d1502120c070554db4bba7b3e40d066fcaecc85c
# AURELIA FL plots ----------------------------------------------------------------
#comparing FL inshore and shelf
fl.shelf$pop_delta_mean_no_m3 = ifelse(fl.shelf$year == 1985, NA, fl.shelf$pop_delta_mean_no_m3)
fl.insh$pop_delta_mean_no_m3 = ifelse(fl.insh$year == 1985, NA, fl.insh$pop_delta_mean_no_m3)
flcp <- ggplot() +
  # flin
  geom_point(data=subset(fl.insh, fl.insh$year >=1984), aes(x=year,y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Inshore"), shape = 4, size = 2) +
  geom_line(data=subset(fl.insh, fl.insh$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE, colour="Inshore"), linetype = "dotted") +
  # flsh
  geom_point(data=subset(fl.shelf, fl.shelf$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf"), shape = 5, size = 2) +
  geom_line(data=subset(fl.shelf, fl.shelf$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf"), linetype = "solid") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0,4,0.5), limits = c(0,4)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  ggtitle("Aurelia_Florida_1984-2018") +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  labs(x= "Year", y=expression(paste(" logTaxa Density ", (kg/m^3)))) +
  scale_color_manual(name="Subregion_Alongshore", values = c(Shelf="coral3", Inshore="darkseagreen4")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 16)) +
  guides(colour = guide_legend(override.aes = list(
    linetype = c("dotted", "solid"),
    shape = c(4,5),
    color = c("darkseagreen4","coral3")))) +
  theme(legend.background = element_rect(fill = NA),
<<<<<<< HEAD
        legend.position = c(0.9, 0.9), legend.key = element_rect(fill = NA)) +
  theme(axis.ticks = element_line(size = 0.5),
        axis.text.x = element_text(size = 0)) +
  theme( axis.line.x = element_line(colour = "white",
                                    size = 1, linetype = "solid"))
=======
        legend.position = c(0.9, 0.9), legend.key = element_rect(fill = NA))
>>>>>>> d1502120c070554db4bba7b3e40d066fcaecc85c
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
  geom_point(data=subset(la.insh, la.insh$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Inshore"), shape = 4, size = 2) +
  geom_line(data=subset(la.insh, la.insh$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Inshore"), linetype = "dotted") +
  # lash
  geom_point(data=subset(la.shelf, la.shelf$year >=1984), aes(x=year,y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf"), shape = 5, size = 2) +
  geom_line(data=subset(la.shelf, la.shelf$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf"), linetype = "solid") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0,4,0.5), limits = c(0,4)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  labs(x= "Year", y=expression(paste(" logTaxa Density ", (kg/m^3)))) +
  scale_color_manual(name="Subregion_Alongshore", values = c(Shelf="coral3", Inshore="darkseagreen4")) +
  ggtitle("Aurelia_Louisiana_1984-2018")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 16)) +
  guides(colour = guide_legend(override.aes = list(
    linetype = c("dotted", "solid"),
    shape = c(4,5),
    color = c("darkseagreen4","coral3")))) +
  theme(legend.background = element_rect(fill = NA),
<<<<<<< HEAD
        legend.position = c(0.9, 0.9), legend.key = element_rect(fill = NA)) +
  theme(axis.ticks = element_line(size = 0.5),
        axis.text.x = element_text(size = 0)) +
  theme( axis.line.x = element_line(colour = "white",
                                    size = 1, linetype = "solid"))
=======
        legend.position = c(0.9, 0.9), legend.key = element_rect(fill = NA))
>>>>>>> d1502120c070554db4bba7b3e40d066fcaecc85c
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
  geom_point(data=subset(tx.insh, tx.insh$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Inshore"), shape = 4, size = 2) +
  geom_line(data=subset(tx.insh, tx.insh$year >=1984), aes(x=year,y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Inshore"), linetype = "dotted") +
  # txsh
  geom_point(data=subset(tx.shelf, tx.shelf$year >=1984), aes(x=year,y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf"), shape = 5, size = 2) +
  geom_line(data=subset(tx.shelf, tx.shelf$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf"), linetype = "solid") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0,4,0.5), limits = c(0,4)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  labs(x= "Year", y=expression(paste(" logTaxa Density ", (kg/m^3)))) +
  scale_color_manual(name="Subregion_Alongshore", values = c(Shelf="coral3", Inshore="darkseagreen4")) +
  theme(plot.title = element_text(size = 16, hjust = 0.5)) +
  ggtitle("Aurelia_Texas_1984-2018") +
  guides(colour = guide_legend(override.aes = list(
    linetype = c("dotted", "solid"),
    shape = c(4,5),
    color = c("darkseagreen4","coral3")))) +
  theme(legend.background = element_rect(fill = NA),
<<<<<<< HEAD
        legend.position = c(0.9, 0.9), legend.key = element_rect(fill = NA)) +
  theme(axis.ticks = element_line(size = 0.5),
        axis.text.x = element_text(size = 0)) +
  theme( axis.line.x = element_line(colour = "white",
                                    size = 1, linetype = "solid"))
=======
        legend.position = c(0.9, 0.9), legend.key = element_rect(fill = NA))
>>>>>>> d1502120c070554db4bba7b3e40d066fcaecc85c
file = "AURELIA_TX.png"
png(file=file, width=12,height=9,units="in", res=225)
plot(txcp)
dev.off()







<<<<<<< HEAD





=======
>>>>>>> d1502120c070554db4bba7b3e40d066fcaecc85c
# AURELIA and CHRYSAORA whole gulf ----------------------------------------
wgp2 = ggplot() +
  geom_point(data=subset(yr_stats, yr_stats$year >= 1984), aes(x=year,y=log(pop_delta_mean_no_m3+1), colour="Aurelia", na.rm=TRUE), shape = 1, size = 2) +
  geom_line(data=subset(yr_stats, yr_stats$year >= 1984), aes(x=year,y=log(pop_delta_mean_no_m3+1), colour="Aurelia", na.rm=TRUE), linetype = "dashed") +
  geom_point(data=subset(yr_stats_chrys, yr_stats_chrys$year >=1984), aes(x=year,y=log(pop_delta_mean_no_m3+1), color="Chrysaora"), shape = 2, size = 2) +
  geom_line(data=subset(yr_stats_chrys, yr_stats_chrys$year >=1984), aes(x=year,y=log(pop_delta_mean_no_m3+1), color="Chrysaora")) +
  theme_classic() +
  labs(x= "Year", y=expression(paste(" Taxa Density ", (kg/m^3)))) +
  scale_y_continuous(breaks = seq(0,4,0.5)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
<<<<<<< HEAD
  scale_color_manual(name="Taxa", values = c("blue","firebrick")) +
=======
  scale_color_manual(name="Taxa", values = c(Aurelia="blue", Chrysoara="firebrick")) +
>>>>>>> d1502120c070554db4bba7b3e40d066fcaecc85c
  ggtitle("Aurelia_and_Chrysaora_WholeGulf_1984-2018") +
  theme(plot.title = element_text(size = 16, hjust = 0.5)) +
  guides(colour = guide_legend(override.aes = list(
    linetype = c("dashed", "solid"),
    shape = c(1,2),
<<<<<<< HEAD
   color = c("Aurelia" = "blue", "Chrysaora" = "firebrick")))) +
  theme(legend.background = element_rect(fill = NA),
        legend.position = c(0.9, 0.9), legend.key = element_rect(fill = NA)) +
  theme(axis.ticks = element_line(size = 0.5),
        axis.text.x = element_text(size = 0)) +
  theme(axis.line.x = element_line(colour = "white",
                                    size = 1, linetype = "solid"))
=======
    color = c("blue","firebrick")))) +
  theme(legend.background = element_rect(fill = NA),
        legend.position = c(0.9, 0.9), legend.key = element_rect(fill = NA))
>>>>>>> d1502120c070554db4bba7b3e40d066fcaecc85c
file = "AURELIA_CHRYSAORA_WHOLEGULF.png"
png(file=file, width=12,height=9,units="in", res=225)
plot(wgp2)
dev.off()





<<<<<<< HEAD




=======
>>>>>>> d1502120c070554db4bba7b3e40d066fcaecc85c
# CHRYSAORA FL plots ----------------------------------------------------------------
#comparing FL inshore and shelf
chrys.fl.shelf$pop_delta_mean_no_m3 = ifelse(chrys.fl.shelf$year == 1985, NA, chrys.fl.shelf$pop_delta_mean_no_m3)
chrys.fl.insh$pop_delta_mean_no_m3 = ifelse(chrys.fl.insh$year == 1985, NA, chrys.fl.insh$pop_delta_mean_no_m3)
cflcp <- ggplot() +
  # flin
  geom_point(data=subset(chrys.fl.insh, chrys.fl.insh$year >=1984), aes(x=year,y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Inshore"),  shape = 4, size = 2) +
  geom_line(data=subset(chrys.fl.insh, chrys.fl.insh$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Inshore"), linetype = "dotted") +
  # flsh
  geom_point(data=subset(chrys.fl.shelf, chrys.fl.shelf$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf"), shape = 5, size = 2) +
  geom_line(data=subset(chrys.fl.shelf, chrys.fl.shelf$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf"), linetype = "solid") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0,4,0.5), limits = c(0,4)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  ggtitle("Chrysaora_Florida_1984-2018") +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  labs(x= "Year", y=expression(paste(" logTaxa Density ", (kg/m^3)))) +
  scale_color_manual(name="Region", values = c(Shelf="coral3", Inshore="darkseagreen4")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 16)) +
  guides(colour = guide_legend(override.aes = list(
    linetype = c("dotted", "solid"),
    shape = c(4,5),
    color = c("darkseagreen4","coral3")))) +
  theme(legend.background = element_rect(fill = NA),
        legend.position = c(0.9, 0.9), legend.key = element_rect(fill = NA))
file = "CHRYSAORA_FL.png"
png(file=file, width=12,height=9,units="in", res=225)
plot(cflcp)
dev.off()


# CHRYSAORA LA plots --------------------------------------------------------
#comparing LA inshore and shelf
chrys.la.shelf$pop_delta_mean_no_m3 = ifelse(chrys.la.shelf$year == 1985, NA, chrys.la.shelf$pop_delta_mean_no_m3)
chrys.la.insh$pop_delta_mean_no_m3 = ifelse(chrys.la.insh$year == 1985, NA, chrys.la.insh$pop_delta_mean_no_m3)
clacp <- ggplot() +
  # lain
  geom_point(data=subset(chrys.la.insh, chrys.la.insh$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Inshore"), shape = 4, size = 2) +
  geom_line(data=subset(chrys.la.insh, chrys.la.insh$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Inshore"), linetype = "dotted") +
  # lash
  geom_point(data=subset(chrys.la.shelf, chrys.la.shelf$year >=1984), aes(x=year,y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf"), shape = 5, size = 2) +
  geom_line(data=subset(chrys.la.shelf, chrys.la.shelf$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf"),  linetype = "solid") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0,4,0.5), limits = c(0,4)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  labs(x= "Year", y=expression(paste(" logTaxa Density ", (kg/m^3)))) +
  scale_color_manual(name="Region", values = c(Shelf="coral3", Inshore="darkseagreen4")) +
  ggtitle("Chrysaora_Louisiana_1984-2018")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 16)) +
  theme(axis.ticks = element_line(size = 0.5),
        axis.text.x = element_text(size = 0)) +
  theme( axis.line.x = element_line(colour = "white",
                                    size = 1, linetype = "solid")) +
  guides(colour = guide_legend(override.aes = list(
    linetype = c("dotted", "solid"),
    shape = c(4,5),
    color = c("darkseagreen4","coral3")))) +
  theme(legend.background = element_rect(fill = NA),
        legend.position = c(0.9, 0.9), legend.key = element_rect(fill = NA))
file = "CHRYSAORA_LA.png"
png(file=file, width=12,height=9,units="in", res=225)
plot(clacp)
dev.off()




# CHRYSAORA TX plots --------------------------------------------------------
#comparing TX inshore and shelf
chrys.tx.shelf$pop_delta_mean_no_m3 = ifelse(chrys.tx.shelf$year == 1985, NA, chrys.tx.shelf$pop_delta_mean_no_m3)
chrys.tx.insh$pop_delta_mean_no_m3 = ifelse(chrys.tx.insh$year == 1985, NA, chrys.tx.insh$pop_delta_mean_no_m3)
ctxcp <- ggplot() +
  # txin
  geom_point(data=subset(chrys.tx.insh, chrys.tx.insh$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Inshore"),  shape = 4, size = 2) +
  geom_line(data=subset(chrys.tx.insh, chrys.tx.insh$year >=1984), aes(x=year,y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Inshore"), linetype = "dotted") +
  # txsh
  geom_point(data=subset(chrys.tx.shelf, chrys.tx.shelf$year >=1984), aes(x=year,y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf"),  shape = 5, size = 2) +
  geom_line(data=subset(chrys.tx.shelf, chrys.tx.shelf$year >=1984), aes(x=year, y=log(pop_delta_mean_no_m3+1), na.rm=TRUE,colour="Shelf"), linetype = "solid") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0,4,0.5), limits = c(0,4)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  labs(x= "Year", y=expression(paste(" logTaxa Density ", (kg/m^3)))) +
  scale_color_manual(name="Region", values = c(Shelf="coral3", Inshore="darkseagreen4")) +
  theme(plot.title = element_text(size = 16, hjust = 0.5)) +
  ggtitle("Chrysaora_Texas_1984-2018") +
  theme(axis.ticks = element_line(size = 0.5),
        axis.text.x = element_text(size = 0)) +
  theme( axis.line.x = element_line(colour = "white",
                                    size = 1, linetype = "solid")) +
  guides(colour = guide_legend(override.aes = list(
    linetype = c("dotted", "solid"),
    shape = c(4,5),
    color = c("darkseagreen4","coral3")))) +
  theme(legend.background = element_rect(fill = NA),
        legend.position = c(0.9, 0.9), legend.key = element_rect(fill = NA))
file = "CHRYSAORA_TX.png"
png(file=file, width=12,height=9,units="in", res=225)
plot(ctxcp)
dev.off()














<<<<<<< HEAD








=======
>>>>>>> d1502120c070554db4bba7b3e40d066fcaecc85c
# TX plot -----------------------------------------------------------------
shore_stats = rbind(yr_shore_stats, yr_shore_stats_chrys)
shore_stats_total = ddply(shore_stats, .(year, subregion_alongshore), summarize,
                          total_pop_delta_mean_no_m3 = sum(pop_delta_mean_no_m3))
txp = ggplot() +
 geom_point(data=subset(shore_stats_total, subregion_alongshore == "1_Tex" & year >= 1984), aes(x=year,y=log(total_pop_delta_mean_no_m3+1))) +
  geom_line(data=subset(shore_stats_total,subregion_alongshore == "1_Tex" & year >= 1984), aes(x=year,y=log(total_pop_delta_mean_no_m3+1))) +
  theme_classic() +
  labs(x= "Year", y=expression(paste(" Taxa Density ", (kg/m^3)))) +
  scale_y_continuous(breaks = seq(0,4,0.5)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  scale_color_manual(values=c("black")) +
  ggtitle("Aurelia_and_Chrysaora_Texas_1984-2018") +
  theme(plot.title = element_text(size = 16, hjust = 0.5)) +
  theme(axis.ticks = element_line(size = 0.5),
        axis.text.x = element_text(size = 0)) +
  theme( axis.line.x = element_line(colour = "white",
                                    size = 1, linetype = "solid"))
file = "AURELIA_CHRYSAORA_TEXAS.png"
png(file=file, width=12,height=9,units="in", res=225)
plot(txp)
dev.off()



# LA plot -----------------------------------------------------------------
lap = ggplot() +
  geom_point(data=subset(shore_stats_total, subregion_alongshore == "2_Lou" & year >= 1984), aes(x=year,y=log(total_pop_delta_mean_no_m3+1))) +
  geom_line(data=subset(shore_stats_total,subregion_alongshore == "2_Lou" & year >= 1984), aes(x=year,y=log(total_pop_delta_mean_no_m3+1))) +
  theme_classic() +
  labs(x= "Year", y=expression(paste(" Taxa Density ", (kg/m^3)))) +
  scale_y_continuous(breaks = seq(0,4,0.5)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  scale_color_manual(values=c("black")) +
  ggtitle("Aurelia_and_Chrysaora_Louisiana_1984-2018") +
  theme(plot.title = element_text(size = 16, hjust = 0.5))  +
  theme(axis.ticks = element_line(size = 0.5),
    axis.text.x = element_text(size = 0)) +
  theme( axis.line.x = element_line(colour = "white",
                                  size = 1, linetype = "solid"))
file = "AURELIA_CHRYSAORA_LOUISIANA.png"
png(file=file, width=12,height=9,units="in", res=225)
plot(lap)
dev.off()



# FL plot -----------------------------------------------------------------
flp = ggplot() +
  geom_point(data=subset(shore_stats_total, subregion_alongshore == "3_Fla" & year >= 1984), aes(x=year,y=log(total_pop_delta_mean_no_m3+1))) +
  geom_line(data=subset(shore_stats_total,subregion_alongshore == "3_Fla" & year >= 1984), aes(x=year,y=log(total_pop_delta_mean_no_m3+1))) +
  theme_classic() +
  labs(x= "Year", y=expression(paste(" Taxa Density ", (kg/m^3)))) +
  scale_y_continuous(breaks = seq(0,4,0.5)) +
  scale_x_continuous(breaks = seq(1984,2018,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  scale_color_manual(values=c("black")) +
  ggtitle("Aurelia_and_Chrysaora_Florida_1984-2018") +
  theme(plot.title = element_text(size = 16, hjust = 0.5))
file = "AURELIA_CHRYSAORA_FLORIDA.png"
png(file=file, width=12,height=9,units="in", res=225)
plot(flp)
dev.off()








