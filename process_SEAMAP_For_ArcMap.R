# PROCESS_SEAMAP_FOR_ARCMAP ---------

# PURPOSE: clean-up output from Fish_Biomass_Step3 (Ruzicka SEAMAP query chain output) in prep for upload into ArcCatalog & ArcMap software

# DATE CREATED: 28 March 2019

# AUTHOR: Kelly L. Robinson

library(plyr)
library(dplyr)
library(data.table)

d <- as.data.frame(fread(input = "Aurelia_SEAMAP.csv", stringsAsFactors = F, sep = ",", header = T))

d$AggGrp_20130430 <- ifelse(test = is.na(d$AggGrp_20130430), "large jellyfish", d$AggGrp_20130430)

d <- d[,c("AggGrp_20130430","Year", "Month", "Day","STATIONID","Subregion_Depth","Subregion_alongshore",
          "Use_Pop_Den_no_m2","Use_Biomass_Den_kg_m2","Use_Depth_m", "DECSLAT","DECSLON","DECELAT","DECELON"),]

names(d) <- tolower(names(d))
d <- plyr::rename(d, c("use_depth_m" = "depth_m"))
d <- plyr::rename(d, c("agggrp_20130430" = "agg_grp"))
d <- d[!is.na(d$depth_m),] #Aurelia: no of records drops from 28,622 to 26,740 (lost 6.6% of total records)

d$biomass_den_kg.m3 <- d$use_biomass_den_kg_m2*d$depth_m
d$pop_den_no.m3 <- d$use_pop_den_no_m2*d$depth_m

d$biomass_den_kg.m3 <- ifelse(test = is.na(d$biomass_den_kg.m3), 0, d$biomass_den_kg.m3)
d$pop_den_no.m3 <- ifelse(test = is.na(d$pop_den_no.m3), 0, d$pop_den_no.m)

f_mean_lat <- function(x){

  decelat <- as.numeric(getElement(x, "decelat"))
  decslat <- as.numeric(getElement(x, "decslat"))

  if(!is.na(decelat)){
    decslat_ctr <- (decslat + decelat)/2
  } else {
    decslat_ctr <- decslat
  }
  return(decslat_ctr)

}
f_mean_lon <- function(x){

  decelon <- as.numeric(getElement(x, "decelon"))
  decslon <- as.numeric(getElement(x, "decslon"))

  if(!is.na(decelon)){
    decslon_ctr <- (decslon + decelon)/2
  } else {
    decslon_ctr <- decslon
  }

  return(decslon_ctr)

}

d$decslat_ctr <- apply(X = d, MARGIN = 1, FUN = f_mean_lat)
d$decslon_ctr <- apply(X = d, MARGIN = 1, FUN = f_mean_lon)

d$decslat <- NULL
d$decelat <- NULL
d$decslon <- NULL
d$decelon <- NULL

write.table(d, file = paste0(unique(d$agg_grp), "_arc.txt"), sep =  "\t", col.names = T, row.names = F)
