
# PROCESS_SEAMAP_FROM_FISHNET ---------

# PURPOSE: Generate time-series of annual mean biomass (kg WWT/m3) and abundance (no/m4) from spatial join data created with fishnet. (This is the same as doing "Step 2" &
#          'Step 3" in the JMP processing of SEAMAP data.)

# DATE CREATED: 28 March 2019

# AUTHOR: Kelly L. Robinson

# -----------

library(plyr)
library(dplyr)
library(data.table)
source("f_delta_stats.R")


d <- as.data.frame(fread(input = "jelly_SEAMAP_30min.txt", sep = ",", stringsAsFactors = F))
names(d) <- tolower(names(d))

#d <- d[complete.cases(d),] #remove the 3 cases of NAs

d <- d[,c(4:length((d)))]
d <- plyr::rename(d, c("join_fid" = "cell_id"))

#REad in Cell Areas -----
a <- as.data.frame(fread(input = "30minCellArea.txt", sep = ",", stringsAsFactors = F))
names(a) <- tolower(names(a))

a <- a[,c(1,3)]
a <- plyr::rename(a, c("objectid" = "cell_id"))

#Read in Cell Center XY coordinates -----------
c <- as.data.frame(fread(input = "30minCell_CtrPts.txt", sep = ",", stringsAsFactors = F))
names(c) <- tolower(names(c))
c <- plyr::rename(c, c("point_x" = "decslat_cell_ctr"))
c <- plyr::rename(c, c("point_y" = "decslon_cell_ctr"))
c <- plyr::rename(c, c("orig_fid" = "cell_id"))
c <- c[,c(3:5)]

# Merge all of the files above together --------
ac <- merge(a, c ,by = "cell_id")
m <- merge(d, ac ,by = "cell_id")
m <- plyr::rename(m, c("shape_area" = "area_m2"))

#calculate annual mean biomass & population density in each fish net cell | STEP 2 in JMP workflow

yr_cell_stats <- ddply(m, .variables = c("cell_id","year"), function(x){

  year <- unique(x$year)
  cell_id <- unique(x$cell_id)
  agg_grp <- unique(x$agg_grp)
  area_m2 <- unique(x$area_m2)
  decslat_cell_ctr <- unique(x$decslat_cell_ctr)
  decslon_cell_ctr <- unique(x$decslon_cell_ctr)


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

    total_bio_wwt_kg_mean <- bio_mean_kg*area_m2
    total_bio_wwt_kg_var <- bio_var_kg*area_m2

    total_indiv_mean <- pop_mean_no_m3*area_m2
    total_indiv_var <- pop_var_no_m3*area_m2

    y <- data.frame(cell_id, decslat_cell_ctr, decslon_cell_ctr, area_m2, year, agg_grp, bio_mean_kg, bio_var_kg, bio_CI95_kg, total_bio_wwt_kg_mean, total_bio_wwt_kg_var,
                    pop_mean_no_m3, pop_var_no_m3, pop_CI95_no_m3, total_indiv_mean, total_indiv_var, n_obs)

    } else {

    bio_mean_kg <- 0
    bio_var_kg <- 0
    bio_CI95_kg <- 0

    pop_mean_no_m3 <- 0
    pop_var_no_m3 <- 0
    pop_CI95_no_m3 <-0

    n_obs <- nrow(x)

    cell_area_m2 <- unique(x$area_m2)

    total_bio_wwt_kg_mean <- bio_mean_kg*area_m2
    total_bio_wwt_kg_var <- bio_var_kg*area_m2

    total_indiv_mean <- pop_mean_no_m3*area_m2
    total_indiv_var <- pop_var_no_m3*area_m2

    y <- data.frame(cell_id, decslat_cell_ctr, decslon_cell_ctr, area_m2, year, agg_grp, bio_mean_kg, bio_var_kg, bio_CI95_kg, total_bio_wwt_kg_mean, total_bio_wwt_kg_var,
                    pop_mean_no_m3, pop_var_no_m3, pop_CI95_no_m3, total_indiv_mean, total_indiv_var, n_obs)


    }

  }

  return(y)

}, .progress = "text", .inform = T)

yr_cell_stats$agg_grp <- as.character(yr_cell_stats$agg_grp)

#data subsetted to send to Dr. Hui Lui @ TAMU-CC
j <- yr_cell_stats[yr_cell_stats$year > 2011,]
j <- plyr::rename(j, c("agg_grp" = "taxa_grp"))
j$taxa_grp <- "Aurelia_spp"
j <- j[,c("cell_id", "decslat_cell_ctr", "decslon_cell_ctr","year","taxa_grp","pop_mean_no_m3", "pop_var_no_m3","n_obs")]


#calculate annual mean biomass & population density | STEP 2 in JMP workflow | STEP 3 in JMP workflow

yr_stats <- ddply(yr_cell_stats, .variables = c("year"), function(x){

  year <- unique(x$year)
  agg_grp <- unique(x$agg_grp)

  if(nrow(x)>0){

    if(sum(x$total_bio_wwt_kg_mea)>0){

      bio_stat <- f_delta_stat(x = x$total_bio_wwt_kg_mean)
      bio_mean_kg <- bio_stat$mean
      bio_var_kg <- sum(x$ total_bio_wwt_kg_var)
      bio_sd_kg <- bio_var_kg^2

      pop_stat <- f_delta_stat(x = x$pop_mean_no_m3)
      pop_mean_no_m3 <- pop_stat$mean
      pop_var_no_m3 <-  sum(x$pop_var_no_m3)
      pop_sd_kg <- pop_var_no_m3^2

      n_obs <- nrow(x)

      y <- data.frame(year, agg_grp, bio_mean_kg, bio_var_kg, bio_sd_kg, pop_mean_no_m3, pop_var_no_m3, pop_sd_kg, n_obs)

    } else {

      bio_mean_kg <- 0
      bio_var_kg <-0
      bio_sd_kg <- bio_var_kg^2

      pop_mean_no_m3 <- 0
      pop_var_no_m3 <-  0
      pop_sd_kg <- pop_var_no_m3^2

      n_obs <- nrow(x)

      y <- data.frame(year, agg_grp,bio_mean_kg, bio_var_kg, bio_sd_kg, pop_mean_no_m3, pop_var_no_m3, pop_sd_kg, n_obs)

    }

  }

  return(y)

}, .progress = "text", .inform = T)
