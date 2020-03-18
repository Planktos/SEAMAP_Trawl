
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


d <- as.data.frame(fread(input = "aurelia_15minCell_statareas.txt", sep = ",", stringsAsFactors = F))
names(d) <- tolower(names(d))

#d <- d[complete.cases(d),] #remove the 3 cases of NAs

d <- d[,c(4:length((d)))]
d <- plyr::rename(d, c("join_fid" = "cell_id"))

#REad in Cell Areas -----
a <- as.data.frame(fread(input = "15minCellArea.txt", sep = ",", stringsAsFactors = F))
names(a) <- tolower(names(a))

a <- a[,c(1,3,5:6)]
a <- plyr::rename(a, c("objectid" = "cell_id"))
a <- plyr::rename(a, c("longitude" = "decslon_cell_ctr"))
a <- plyr::rename(a, c("latitude" = "decslat_cell_ctr"))


m <- merge(d, a ,by = "cell_id")
m <- plyr::rename(m, c("shape_area" = "area_m2"))

#calculate annual mean biomass & population density in each fish net cell | STEP 2 in JMP workflow

yr_cell_stats <- ddply(m, .variables = c("cell_id","year"), function(x){

  year <- unique(x$year)
  cell_id <- unique(x$cell_id)
  agg_grp <- unique(x$agg_grp)
  area_m2 <- unique(x$area_m2)
  decslat_cell_ctr <- unique(x$decslat_cell_ctr)
  decslon_cell_ctr <- unique(x$decslon_cell_ctr)
  subregion_depth <- unique(x$subregion_depth)
  subregion_alongshore <- unique(x$subregion_alongshore)


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

    y <- data.frame(cell_id, decslat_cell_ctr, decslon_cell_ctr, area_m2, year, agg_grp, subregion_depth, subregion_alongshore, bio_mean_kg, bio_var_kg, bio_CI95_kg, total_bio_wwt_kg_mean, total_bio_wwt_kg_var,
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

    y <- data.frame(cell_id, decslat_cell_ctr, decslon_cell_ctr, area_m2, year, agg_grp, subregion_depth, subregion_alongshore, bio_mean_kg, bio_var_kg, bio_CI95_kg, total_bio_wwt_kg_mean, total_bio_wwt_kg_var,
                    pop_mean_no_m3, pop_var_no_m3, pop_CI95_no_m3, total_indiv_mean, total_indiv_var, n_obs)


    }

  }

  return(y)

}, .progress = "text", .inform = T)

yr_cell_stats$agg_grp <- as.character(yr_cell_stats$agg_grp)

# #data subsetted to send to Dr. Hui Lui @ TAMU-CC
# j <- yr_cell_stats[yr_cell_stats$year > 2011,]
# j <- plyr::rename(j, c("agg_grp" = "taxa_grp"))
# j$taxa_grp <- "Aurelia_spp"
# j <- j[,c("cell_id", "decslat_cell_ctr", "decslon_cell_ctr","year","taxa_grp","pop_mean_no_m3", "pop_var_no_m3","n_obs")]
#
# write.csv(x = j, file = "Aurliea_GoMex_SEAMAP_2011-2017_30minCells.csv", row.names = F)


#calculate annual mean biomass & population density | STEP 2 in JMP workflow | STEP 3 in JMP workflow

# Whole Gulf of Mexico time-series ------
yr_stats <- ddply(yr_cell_stats, .variables = c("year"), function(x){

  year <- unique(x$year)
  agg_grp <- unique(x$agg_grp)

  if(nrow(x)>0){

    if(sum(x$total_bio_wwt_kg_mea)>0){

      bio_stat <- f_delta_stat(x = x$total_bio_wwt_kg_mean)
      bio_delta_mean_kg <- bio_stat$mean
      bio_wt_mean_kg <- weighted.mean(x = x$total_bio_wwt_kg_mean, w = x$n_obs)
      bio_delta_var_kg <- sum(x$ total_bio_wwt_kg_var)
      bio_delta_sd_kg <- bio_delta_var_kg^2

      pop_stat <- f_delta_stat(x = x$pop_mean_no_m3)
      pop_delta_mean_no_m3 <- pop_stat$mean
      pop_delta_var_no_m3 <-  sum(x$pop_var_no_m3)
      pop_delta_sd_no_m3 <- pop_delta_var_no_m3^2
      pop_wt_mean_no_m3 <- weighted.mean(x = x$pop_mean_no_m3, w = x$n_obs)

      n_obs <- nrow(x)

      y <- data.frame(year, agg_grp, bio_wt_mean_kg, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_sd_kg,
                      pop_wt_mean_no_m3, pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_sd_no_m3, n_obs)

    } else {

      bio_delta_mean_kg <- 0
      bio_delta_var_kg <-0
      bio_delta_sd_kg <- bio_delta_var_kg^2
      bio_wt_mean_kg <- 0

      pop_delta_mean_no_m3 <- 0
      pop_delta_var_no_m3 <-  0
      pop_delta_sd_no_m3 <- pop_delta_var_no_m3^2
      pop_wt_mean_no_m3 <- 0

      n_obs <- nrow(x)

      y <- data.frame(year, agg_grp, bio_wt_mean_kg, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_sd_kg,
                      pop_wt_mean_no_m3, pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_sd_no_m3, n_obs)

    }

  }

  return(y)

}, .progress = "text", .inform = T)

p <- plot(x=yr_stats$year, y=log(yr_stats$pop_wt_mean_no_m3), type = "b")
p <- plot(x=yr_stats$year, y=yr_stats$pop_wt_mean_no_m3, type = "b")
p <- plot(x=yr_stats$year, y=yr_stats$bio_delta_mean_kg, type = "b")

model <- lm(data = yr_stats, formula = year ~ log(pop_wt_mean_no_m3))
summary(model)

save(yr_stats, file = "Aurelia_SEAMAP_All_GoM_time-series.Rdata")


# By Subregion_alongshore time-series ------
yr_shore_stats <- ddply(yr_cell_stats, .variables = c("year","subregion_alongshore"), function(x){

  agg_grp <- unique(x$agg_grp)

  if(nrow(x)>0){

    if(sum(x$total_bio_wwt_kg_mea)>0){

      bio_stat <- f_delta_stat(x = x$total_bio_wwt_kg_mean)
      bio_delta_mean_kg <- bio_stat$mean
      bio_wt_mean_kg <- weighted.mean(x = x$total_bio_wwt_kg_mean, w = x$n_obs)
      bio_delta_var_kg <- sum(x$ total_bio_wwt_kg_var)
      bio_delta_sd_kg <- bio_delta_var_kg^2

      pop_stat <- f_delta_stat(x = x$pop_mean_no_m3)
      pop_delta_mean_no_m3 <- pop_stat$mean
      pop_delta_var_no_m3 <-  sum(x$pop_var_no_m3)
      pop_delta_sd_no_m3 <- pop_delta_var_no_m3^2
      pop_wt_mean_no_m3 <- weighted.mean(x = x$pop_mean_no_m3, w = x$n_obs)

      n_obs <- nrow(x)

      y <- data.frame(agg_grp, bio_wt_mean_kg, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_sd_kg,
                      pop_wt_mean_no_m3, pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_sd_no_m3, n_obs)

    } else {

      bio_delta_mean_kg <- 0
      bio_delta_var_kg <-0
      bio_delta_sd_kg <- bio_delta_var_kg^2
      bio_wt_mean_kg <- 0

      pop_delta_mean_no_m3 <- 0
      pop_delta_var_no_m3 <-  0
      pop_delta_sd_no_m3 <- pop_delta_var_no_m3^2
      pop_wt_mean_no_m3 <- 0

      n_obs <- nrow(x)

      y <- data.frame(agg_grp, bio_wt_mean_kg, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_sd_kg,
                      pop_wt_mean_no_m3, pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_sd_no_m3, n_obs)

    }

  }

  return(y)

}, .progress = "text", .inform = T)

save(yr_shore_stats, file = "Aurelia_SEAMAP_GoM_region_time-series.Rdata")


  # TEXAS
  tx <- yr_shore_stats[yr_shore_stats$subregion_alongshore == "1_Tex",]

  p <- plot(x=tx$year, y=log(tx$pop_wt_mean_no_m3+1), type = "b")
  p <- plot(x=tx$year, y=tx$pop_wt_mean_no_m3, type = "b")
  p <- plot(x=tx$year, y=tx$bio_delta_mean_kg, type = "b")

  model.tx <- lm(data = tx, formula = year ~ log(pop_wt_mean_no_m3+1))
  summary(model.tx) #Not significant change over time

  # LOUISIANA
  la <- yr_shore_stats[yr_shore_stats$subregion_alongshore == "2_Lou",]

  p <- plot(x=la$year, y=log(la$pop_wt_mean_no_m3+1), type = "b")
  p <- plot(x=la$year, y=la$pop_wt_mean_no_m3, type = "b")
  p <- plot(x=la$year, y=la$bio_delta_mean_kg, type = "b")

  model.la <- lm(data = la, formula = year ~ log(pop_wt_mean_no_m3+1))
  summary(model.la) #Not significant change over time

  # FLORIDA
  fl <- yr_shore_stats[yr_shore_stats$subregion_alongshore == "3_Fla",]

  p <- plot(x=fl$year, y=log(fl$pop_wt_mean_no_m3+1), type = "b")
  p <- plot(x=fl$year, y=fl$n_obs, type = "b")

  fl.nfive <- fl[fl$n_obs > 5,]

  p <- plot(x=fl.nfive$year, y=log(fl.nfive$pop_wt_mean_no_m3+1), type = "b")
  p <- plot(x=fl.nfive$year, y=fl.nfive$n_obs, type = "b")

  model.fl <- lm(data = fl.nfive, formula = year ~ log(pop_wt_mean_no_m3+1))
  summary(model.fl) #Significant change over time (p == 0.001)


# By Subregion_depth time-series ------
 yr_depth_stats <- ddply(yr_cell_stats, .variables = c("year","subregion_depth"), function(x){

    agg_grp <- unique(x$agg_grp)

    if(nrow(x)>0){

      if(sum(x$total_bio_wwt_kg_mea)>0){

        bio_stat <- f_delta_stat(x = x$total_bio_wwt_kg_mean)
        bio_delta_mean_kg <- bio_stat$mean
        bio_wt_mean_kg <- weighted.mean(x = x$total_bio_wwt_kg_mean, w = x$n_obs)
        bio_delta_var_kg <- sum(x$ total_bio_wwt_kg_var)
        bio_delta_sd_kg <- bio_delta_var_kg^2

        pop_stat <- f_delta_stat(x = x$pop_mean_no_m3)
        pop_delta_mean_no_m3 <- pop_stat$mean
        pop_delta_var_no_m3 <-  sum(x$pop_var_no_m3)
        pop_delta_sd_no_m3 <- pop_delta_var_no_m3^2
        pop_wt_mean_no_m3 <- weighted.mean(x = x$pop_mean_no_m3, w = x$n_obs)

        n_obs <- nrow(x)

        y <- data.frame(agg_grp, bio_wt_mean_kg, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_sd_kg,
                        pop_wt_mean_no_m3, pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_sd_no_m3, n_obs)

      } else {

        bio_delta_mean_kg <- 0
        bio_delta_var_kg <-0
        bio_delta_sd_kg <- bio_delta_var_kg^2
        bio_wt_mean_kg <- 0

        pop_delta_mean_no_m3 <- 0
        pop_delta_var_no_m3 <-  0
        pop_delta_sd_no_m3 <- pop_delta_var_no_m3^2
        pop_wt_mean_no_m3 <- 0

        n_obs <- nrow(x)

        y <- data.frame(agg_grp, bio_wt_mean_kg, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_sd_kg,
                        pop_wt_mean_no_m3, pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_sd_no_m3, n_obs)

      }

    }

    return(y)

  }, .progress = "text", .inform = T)

  save(yr_depth_stats, file = "Aurelia_SEAMAP_GoM_depth_time-series.Rdata")

  # INSHORE
  insh <- yr_depth_stats[yr_depth_stats$subregion_depth == "1_inshore",]

  p <- plot(x=insh$year, y=log(insh$pop_wt_mean_no_m3+1), type = "b")
  p <- plot(x=insh$year, y=insh$pop_wt_mean_no_m3, type = "b")
  p <- plot(x=insh$year, y=insh$bio_delta_mean_kg, type = "b")

  model.insh <- lm(data = insh, formula = year ~ log(pop_wt_mean_no_m3+1))
  summary(model.insh) #Not Significant change over time

  # SHELF
  shelf <- yr_depth_stats[yr_depth_stats$subregion_depth == "2_shelf",]

  p <- plot(x=shelf$year, y=log(shelf$pop_wt_mean_no_m3+1), type = "b")
  p <- plot(x=shelf$year, y=shelf$pop_wt_mean_no_m3, type = "b")
  p <- plot(x=shelf$year, y=shelf$bio_delta_mean_kg, type = "b")

  model.shelf <- lm(data = shelf, formula = year ~ log(pop_wt_mean_no_m3+1))
  summary(model.shelf) #significant change over time, p = 0.0308

  # OCEANIC - no observations at depths > 200m
  off <- yr_depth_stats[yr_depth_stats$subregion_alongshore == "3_oceanic",]

# By Subregion_alongshore & depth time-series ------
  yr_shore_depth_stats <- ddply(yr_cell_stats, .variables = c("year","subregion_depth","subregion_alongshore"), function(x){

    agg_grp <- unique(x$agg_grp)

    if(nrow(x)>0){

      if(sum(x$total_bio_wwt_kg_mea)>0){

        bio_stat <- f_delta_stat(x = x$total_bio_wwt_kg_mean)
        bio_delta_mean_kg <- bio_stat$mean
        bio_wt_mean_kg <- weighted.mean(x = x$total_bio_wwt_kg_mean, w = x$n_obs)
        bio_delta_var_kg <- sum(x$ total_bio_wwt_kg_var)
        bio_delta_sd_kg <- bio_delta_var_kg^2

        pop_stat <- f_delta_stat(x = x$pop_mean_no_m3)
        pop_delta_mean_no_m3 <- pop_stat$mean
        pop_delta_var_no_m3 <-  sum(x$pop_var_no_m3)
        pop_delta_sd_no_m3 <- pop_delta_var_no_m3^2
        pop_wt_mean_no_m3 <- weighted.mean(x = x$pop_mean_no_m3, w = x$n_obs)

        n_obs <- nrow(x)

        y <- data.frame(agg_grp, bio_wt_mean_kg, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_sd_kg,
                        pop_wt_mean_no_m3, pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_sd_no_m3, n_obs)

      } else {

        bio_delta_mean_kg <- 0
        bio_delta_var_kg <-0
        bio_delta_sd_kg <- bio_delta_var_kg^2
        bio_wt_mean_kg <- 0

        pop_delta_mean_no_m3 <- 0
        pop_delta_var_no_m3 <-  0
        pop_delta_sd_no_m3 <- pop_delta_var_no_m3^2
        pop_wt_mean_no_m3 <- 0

        n_obs <- nrow(x)

        y <- data.frame(agg_grp, bio_wt_mean_kg, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_sd_kg,
                        pop_wt_mean_no_m3, pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_sd_no_m3, n_obs)

      }

    }

    return(y)

  }, .progress = "text", .inform = T)

  # TX - INSHORE
  tx.insh <- yr_shore_depth_stats[yr_shore_depth_stats$subregion_depth == "1_inshore" & yr_shore_depth_stats$subregion_alongshore == "2_Lou",]

  p <- plot(x=tx.insh$year, y=log(tx.insh$pop_wt_mean_no_m3+1), type = "b")
  p <- plot(x=tx.insh$year, y=tx.insh$n_obs, type = "b")

  model.tx.insh <- lm(data = tx.insh, formula = year ~ log(pop_wt_mean_no_m3+1))
  summary(model.tx.insh) #Not Significant change over time

  # TX - SHELF
  tx.shelf <- yr_depth_stats[yr_shore_depth_stats$subregion_depth == "2_shelf" & yr_shore_depth_stats$subregion_alongshore == "1_Tex",]

  p <- plot(x=tx.shelf$year, y=log(tx.shelf$pop_wt_mean_no_m3+1), type = "b")
  p <- plot(x=tx.shelf$year, y=tx.shelf$n_obs, type = "b")

  model.tx.shelf <- lm(data = tx.shelf, formula = year ~ log(pop_wt_mean_no_m3+1))
  summary(model.tx.shelf) #Not significant change over time



  # LA - INSHORE
  la.insh <- yr_shore_depth_stats[yr_shore_depth_stats$subregion_depth == "1_inshore" & yr_shore_depth_stats$subregion_alongshore == "2_Lou",]

  p <- plot(x=la.insh$year, y=log(la.insh$pop_wt_mean_no_m3+1), type = "b")
  p <- plot(x=la.insh$year, y=la.insh$n_obs, type = "b")

  model.la.insh <- lm(data = la.insh, formula = year ~ log(pop_wt_mean_no_m3+1))
  summary(model.la.insh) #Not Significant change over time

  # LA - SHELF
  la.shelf <- yr_depth_stats[yr_shore_depth_stats$subregion_depth == "2_shelf" & yr_shore_depth_stats$subregion_alongshore == "2_Lou",]

  p <- plot(x=la.shelf$year, y=log(la.shelf$pop_wt_mean_no_m3+1), type = "b")
  p <- plot(x=la.shelf$year, y=la.shelf$n_obs, type = "b")

  model.la.shelf <- lm(data = la.shelf, formula = year ~ log(pop_wt_mean_no_m3+1))
  summary(model.la.shelf) #Not significant change over time



  # FL - INSHORE
  fl.insh <- yr_shore_depth_stats[yr_shore_depth_stats$subregion_depth == "1_inshore" & yr_shore_depth_stats$subregion_alongshore == "3_Fla",]

  p <- plot(x=fl.insh$year, y=log(fl.insh$pop_wt_mean_no_m3+1), type = "b")
  p <- plot(x=fl.insh$year, y=fl.insh$n_obs, type = "b")

  model.fl.insh <- lm(data = fl.insh, formula = year ~ log(pop_wt_mean_no_m3+1))
  summary(model.fl.insh) # Significant change over time, p = 0.00585

  # L - SHELF
  fl.shelf <- yr_depth_stats[yr_shore_depth_stats$subregion_depth == "2_shelf" & yr_shore_depth_stats$subregion_alongshore == "3_Fla",]

  p <- plot(x=fl.shelf$year, y=log(fl.shelf$pop_wt_mean_no_m3+1), type = "b")
  p <- plot(x=fl.shelf$year, y=fl.shelf$n_obs, type = "b")

  model.fl.shelf <- lm(data = fl.shelf, formula = year ~ log(pop_wt_mean_no_m3+1))
  summary(model.fl.shelf) #Not significant change over time, p = 0.111

