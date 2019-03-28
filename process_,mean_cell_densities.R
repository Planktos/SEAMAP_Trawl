
# PROCESS_SEAMAP_FROM_FISHNET ---------

# PURPOSE: Generate time-series of annual mean biomass (kg WWT/m3) and abundance (no/m4) from spatial join data created with fishnet

# DATE CREATED: 28 March 2019

# AUTHOR: Kelly L. Robinson

# -----------

library(plyr)
library(dplyr)
library(data.table)
source("f_delta_stats.R")


d <- as.data.frame(fread(input = "jellyfish_SEAMAP_30min.txt", sep = ",", stringsAsFactors = F))
names(d) <- tolower(names(d))
d$agggrp_20130430 <- gsub(x = d$agggrp_20130430, pattern = "_", replacement = " ")

d <- d[,c(4:length((d)))]
d <- plyr::rename(d, c("join_fid" = "cell_id"))

a <- as.data.frame(fread(input = "30minCellArea.txt", sep = ",", stringsAsFactors = F))
names(a) <- tolower(names(a))

a <- a[,c(1,3)]
a <- plyr::rename(a, c("objectid" = "cell_id"))

m <- merge(d, a ,by = "cell_id")
m <- plyr::rename(m, c("shape_area" = "area_m2"))

#calculate annual mean biomass & population density

yr.stats <- ddply(m, .variables = c("cell_id","year"), function(x){

  year <- unique(x$year)
  cell_id <- unique(x$cell_id)
  agg_grp <- unique(x$agggrp_20130430)
  season <- unique(x$season)

  if(nrow(x)>0){

    if(sum(x$biomass_den_kg_m3)>0){

    bio_stat <- f_delta_stat(x = x$biomass_den_kg_m3)
    bio_mean_kg <- bio_stat$mean
    bio_var_kg <- bio_stat$variance
    bio_CI95_kg <- bio_stat$CI95

    pop_stat <- f_delta_stat(x = x$pop_den_no_m3)
    pop_mean_no_m3 <- pop_stat$mean
    pop_var_no_m3 <- pop_stat$variance
    pop_CI95_no_m3 <- pop_stat$CI95

    n_obs <- nrow(x)

    cell_area_m2 <- unique(x$area_m2)

    total_bio_wwt_kg <- bio_mean*area
    total_indiv <- pop.mean*area

    y <- data.frame(cell_id, cell_area_m2, year, agg_grp, season, bio_mean_kg, bio_var_kg, bio_CI95_kg, total_bio_wwt_kg, pop_mean_no_m3, pop_var_no_m3, pop_CI95_no_m3, total_indiv, n_obs)

    } else {

    bio_mean_kg <- 0
    bio_var_kg <- 0
    bio_CI95_kg <- 0

    pop_mean_no_m3 <- 0
    pop_var_no_m3 <- 0
    pop_CI95_no_m3 <-0

    n_obs <- nrow(x)

    cell_area_m2 <- unique(x$area_m2)

    total_bio_wwt_kg <- 0
    total_indiv <- 0

    y <- data.frame(cell_id, cell_area_m2, year, agg_grp, season, bio_mean_kg, bio_var_kg, bio_CI95_kg, total_bio_wwt_kg, pop_mean_no_m3, pop_var_no_m3, pop_CI95_no_m3, total_indiv, n_obs)


    }

  }

  return(y)


}, .progress = "text", .inform = T)