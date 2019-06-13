# PROCESS_SEAMAP_FOR_ARCMAP ---------

# PURPOSE: clean-up output from Fish_Biomass_Step3 (Ruzicka SEAMAP query chain output) in prep for upload into ArcCatalog & ArcMap software

# DATE CREATED: 28 March 2019

# AUTHOR: Kelly L. Robinson

library(plyr)
library(dplyr)
library(data.table)

#taxa = c("AURELIA", "AURELIA AURITA")
taxa = c("CHRYSAORA","CHRYSAORA QUINQUECIRRHA","DACTYLOMETRA QUINQUECIRRHA")


d <- as.data.frame(fread(input = paste0(taxa[1],"_SEAMAP.csv"), stringsAsFactors = F, sep = ",", header = T))

d <- d[,c("taxa","AggGrp_20130430","Season", "Year", "Month", "Day","STATIONID","Subregion_Depth","Subregion_alongshore",
          "Use_Pop_Den_no_m2","Use_Biomass_Den_kg_m2","Use_Depth_m", "DECSLAT","DECSLON","DECELAT","DECELON"),]

names(d) <- tolower(names(d))
d <- plyr::rename(d, c("use_depth_m" = "depth_m"))
d <- plyr::rename(d, c("agggrp_20130430" = "agg_grp"))

grp.name <- as.data.frame(unique(d$agg_grp))
colnames(grp.name) <- c("name")
grp.name <- as.character(grp.name[!is.na(grp.name$name),])

#assign agg_grp name to records with NA values if dealing with only one agg_group
if(length(grp.name) == 1){

  d$agg_grp <- with(d, ifelse(is.na(d$agg_grp), grp.name, d$agg_grp))

  }

d$biomass_den_kg_m3 <- d$use_biomass_den_kg_m2*d$depth_m
d$pop_den_no_m3 <- d$use_pop_den_no_m2*d$depth_m

d$biomass_den_kg_m3 <- ifelse(test = is.na(d$biomass_den_kg_m3), 0, d$biomass_den_kg_m3)
d$pop_den_no_m3 <- ifelse(test = is.na(d$pop_den_no_m3), 0, d$pop_den_no_m3)

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

d[, c("decslat", "decelat", "decslon", "decelon")] = list(NULL)
d$taxa <- taxa[1]

d <- d %>% select(taxa, agg_grp, stationid, year, season, month, day, depth_m, decslat_ctr, decslon_ctr, everything())

write.csv(x = d, file = paste0(taxa[1], "_arc.csv"), row.names = F)
