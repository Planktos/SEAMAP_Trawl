
# PROCESS_SEAMAP_FROM_FISHNET ---------

# PURPOSE: Generate time-series of annual mean biomass (kg WWT/m3) and abundance (no/m4) from spatial join data created with fishnet. (This is the same as doing "Step 2" &
#          'Step 3" in the JMP processing of SEAMAP data.)

# DATE CREATED: 28 March 2019

# AUTHOR: Kelly L. Robinson

# -----------

library(plyr)
library(dplyr)
library(data.table)
library(Hmisc)
source("f_delta_stats.R")

cell_area_min <- 15

#AURELIA -----------

if(cell_area_min == 15){
  d <- as.data.frame(fread(input = "aurelia_15minCell_statareas.txt", sep = ",", stringsAsFactors = F))
} else if(cell_area == 30){
  d <- as.data.frame(fread(input = "aurelia_30minCell_statareas.txt", sep = ",", stringsAsFactors = F))
}

names(d) <- tolower(names(d))

#remove records not associated with a grid cell
d <- d[d$join_count > 0,]

#remove & revise ArcMap fields
d <- d[,c(4:length((d)))]
d <- plyr::rename(d, c("join_fid" = "cell_id"))


#Read in Cell Areas -----
if(cell_area_min == 15){
a<- as.data.frame(fread(input = "15minCellArea.txt", sep = ",", stringsAsFactors = F))
} else if(cell_area == 30){
a<- as.data.frame(fread(input = "30minCellArea.txt", sep = ",", stringsAsFactors = F))
}

names(a) <- tolower(names(a))

a <- a[,c(1,3,5:6)]
a <- plyr::rename(a, c("objectid" = "cell_id"))
a <- plyr::rename(a, c("longitude" = "decslon_cell_ctr"))
a <- plyr::rename(a, c("latitude" = "decslat_cell_ctr"))


m <- merge(d, a ,by = "cell_id")
m <- plyr::rename(m, c("shape_area" = "area_m2"))


#calculate annual mean biomass & population density in each fish net cell ; STEP 2 in JMP workflow
yr_cell_stats <- ddply(m, .variables = c("cell_id","year"), function(x){

  taxa <- unique(x$taxa)
  year <- unique(x$year)
  cell_id <- unique(x$cell_id)
  agg_grp <- unique(x$agg_grp)
  area_m2 <- unique(x$area_m2)
  decslat_cell_ctr <- unique(x$decslat_cell_ctr)
  decslon_cell_ctr <- unique(x$decslon_cell_ctr)
  n_obs <- nrow(x)

  if(n_obs>0){

    if(sum(x$biomass_den_kg_m3)>0){

      cell_area_m2 <- unique(x$area_m2)
      use_pop_den_no_m2 <- x$use_pop_den_no_m2

      if(n_obs>1){

        bio_stat <- f_delta_stat(x = x$biomass_den_kg_m3)
        bio_mean_kg <- bio_stat$mean
        bio_var_kg <- bio_stat$variance
        bio_sd_kg <- sqrt(bio_var_kg)
        bio_CI95_kg <- bio_stat$CI95

        pop_stat <- f_delta_stat(x = x$pop_den_no_m3)
        pop_mean_no_m3 <- pop_stat$mean
        pop_var_no_m3 <- pop_stat$variance
        pop_sd_no_m3 <- sqrt(pop_var_no_m3)
        pop_CI95_no_m3 <- pop_stat$CI95

        total_bio_wwt_kg_mean <- bio_mean_kg*cell_area_m2
        total_bio_wwt_kg_var <- bio_var_kg*cell_area_m2
        total_bio_wwt_kg_sd <- sqrt(total_bio_wwt_kg_var)

        total_indiv_mean <- mean(use_pop_den_no_m2)*cell_area_m2
        total_indiv_var <- var(use_pop_den_no_m2)*cell_area_m2
        total_indiv_sd <- sqrt(total_indiv_var)

        mean_depth_m <- mean(x$depth_m)

        y <- data.frame(agg_grp, taxa, cell_id, decslat_cell_ctr, decslon_cell_ctr, mean_depth_m, area_m2, year, bio_mean_kg, bio_var_kg, bio_sd_kg, bio_CI95_kg,
                        total_bio_wwt_kg_mean, total_bio_wwt_kg_var, total_bio_wwt_kg_sd,
                        pop_mean_no_m3, pop_var_no_m3, pop_sd_no_m3,pop_CI95_no_m3, total_indiv_mean, total_indiv_var,total_indiv_sd, n_obs)

      } else {

        bio_stat <- f_delta_stat(x = x$biomass_den_kg_m3)
        bio_mean_kg <- bio_stat$mean
        bio_var_kg <- NA
        bio_sd_kg <- NA
        bio_CI95_kg <- NA

        pop_stat <- f_delta_stat(x = x$pop_den_no_m3)
        pop_mean_no_m3 <- pop_stat$mean
        pop_var_no_m3 <- NA
        pop_sd_no_m3 <- NA
        pop_CI95_no_m3 <- NA

        n_obs <- nrow(x)

        total_bio_wwt_kg_mean <- bio_mean_kg*area_m2
        total_bio_wwt_kg_var <- NA
        total_bio_wwt_kg_sd <- NA

        total_indiv_mean <- mean(use_pop_den_no_m2)*cell_area_m2
        total_indiv_var <- NA
        total_indiv_sd <- NA

        mean_depth_m <- mean(x$depth_m)

        y <- data.frame(agg_grp, taxa, cell_id, decslat_cell_ctr, decslon_cell_ctr, mean_depth_m, area_m2, year,bio_mean_kg, bio_var_kg, bio_sd_kg, bio_CI95_kg,
                        total_bio_wwt_kg_mean, total_bio_wwt_kg_var, total_bio_wwt_kg_sd,
                        pop_mean_no_m3, pop_var_no_m3, pop_sd_no_m3,pop_CI95_no_m3, total_indiv_mean, total_indiv_var,total_indiv_sd, n_obs)
      }

    } else {

      bio_mean_kg <- 0
      bio_var_kg <- NA
      bio_sd_kg <- NA
      bio_CI95_kg <- NA

      pop_mean_no_m3 <- 0
      pop_var_no_m3 <- NA
      pop_sd_no_m3 <- NA
      pop_CI95_no_m3 <- NA

      n_obs <- nrow(x)

      cell_area_m2 <- unique(x$area_m2)

      total_bio_wwt_kg_mean <- bio_mean_kg*area_m2
      total_bio_wwt_kg_var <- NA
      total_bio_wwt_kg_sd <- NA

      total_indiv_mean <- 0
      total_indiv_var <- NA
      total_indiv_sd <- NA

      mean_depth_m <- mean(x$depth_m)

      y <- data.frame(agg_grp, taxa, cell_id, decslat_cell_ctr, decslon_cell_ctr, mean_depth_m, area_m2, year, bio_mean_kg, bio_var_kg, bio_sd_kg, bio_CI95_kg,
                      total_bio_wwt_kg_mean, total_bio_wwt_kg_var, total_bio_wwt_kg_sd,
                      pop_mean_no_m3, pop_var_no_m3, pop_sd_no_m3,pop_CI95_no_m3, total_indiv_mean, total_indiv_var,total_indiv_sd, n_obs)

    }

  }

  return(y)

}, .progress = "text", .inform = T)


outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}

out <- outlierKD(dt = yr_cell_stats, var = pop_mean_no_m3)

yr_cell_stats$subregion_alongshore = with(yr_cell_stats, ifelse(decslon_cell_ctr < -94, "1_Tex",
                                                                ifelse(decslon_cell_ctr > -88, "3_Fla", "2_Lou")))

yr_cell_stats$subregion_depth = with(yr_cell_stats,
                                     ifelse(mean_depth_m <= 20, "1_inshore",ifelse(is.na(mean_depth_m), NA,
                                                                                  ifelse(mean_depth_m <= 200, "2_shelf", "3_oceanic"))))

yr_cell_stats$agg_grp <- as.character(yr_cell_stats$agg_grp)

#data subsetted to send to Dr. Hui Lui @ TAMU-CC
m <- m[m$season == "fall" | m$season == "summer",]
m <- m[m$year > 2011,]

yr_cell_stats <- ddply(m, .variables = c("cell_id", "year"), function(x){

  taxa <- unique(x$taxa)
  year <- unique(x$year)
  cell_id <- unique(x$cell_id)
  agg_grp <- unique(x$agg_grp)
  area_m2 <- unique(x$area_m2)
  decslat_cell_ctr <- unique(x$decslat_cell_ctr)
  decslon_cell_ctr <- unique(x$decslon_cell_ctr)
  n_obs <- nrow(x)

  if(n_obs>0){

    if(sum(x$biomass_den_kg_m3)>0){

      cell_area_m2 <- unique(x$area_m2)
      use_pop_den_no_m2 <- x$use_pop_den_no_m2

      if(n_obs>1){

        bio_stat <- f_delta_stat(x = x$biomass_den_kg_m3)
        bio_mean_kg <- bio_stat$mean
        bio_var_kg <- bio_stat$variance
        bio_sd_kg <- sqrt(bio_var_kg)
        bio_CI95_kg <- bio_stat$CI95

        pop_stat <- f_delta_stat(x = x$pop_den_no_m3)
        pop_mean_no_m3 <- pop_stat$mean
        pop_var_no_m3 <- pop_stat$variance
        pop_sd_no_m3 <- sqrt(pop_var_no_m3)
        pop_CI95_no_m3 <- pop_stat$CI95

        total_bio_wwt_kg_mean <- bio_mean_kg*cell_area_m2
        total_bio_wwt_kg_var <- bio_var_kg*cell_area_m2
        total_bio_wwt_kg_sd <- sqrt(total_bio_wwt_kg_var)

        total_indiv_mean <- mean(use_pop_den_no_m2)*cell_area_m2
        total_indiv_var <- var(use_pop_den_no_m2)*cell_area_m2
        total_indiv_sd <- sqrt(total_indiv_var)

        mean_depth_m <- mean(x$depth_m)

        y <- data.frame(agg_grp, taxa, cell_id, decslat_cell_ctr, decslon_cell_ctr, mean_depth_m, area_m2, year, bio_mean_kg, bio_var_kg, bio_sd_kg, bio_CI95_kg,
                        total_bio_wwt_kg_mean, total_bio_wwt_kg_var, total_bio_wwt_kg_sd,
                        pop_mean_no_m3, pop_var_no_m3, pop_sd_no_m3,pop_CI95_no_m3, total_indiv_mean, total_indiv_var,total_indiv_sd, n_obs)

      } else {

        bio_stat <- f_delta_stat(x = x$biomass_den_kg_m3)
        bio_mean_kg <- bio_stat$mean
        bio_var_kg <- NA
        bio_sd_kg <- NA
        bio_CI95_kg <- NA

        pop_stat <- f_delta_stat(x = x$pop_den_no_m3)
        pop_mean_no_m3 <- pop_stat$mean
        pop_var_no_m3 <- NA
        pop_sd_no_m3 <- NA
        pop_CI95_no_m3 <- NA

        n_obs <- nrow(x)

        total_bio_wwt_kg_mean <- bio_mean_kg*area_m2
        total_bio_wwt_kg_var <- NA
        total_bio_wwt_kg_sd <- NA

        total_indiv_mean <- mean(use_pop_den_no_m2)*cell_area_m2
        total_indiv_var <- NA
        total_indiv_sd <- NA

        mean_depth_m <- mean(x$depth_m)

        y <- data.frame(agg_grp, taxa, cell_id, decslat_cell_ctr, decslon_cell_ctr, mean_depth_m, area_m2, year,bio_mean_kg, bio_var_kg, bio_sd_kg, bio_CI95_kg,
                        total_bio_wwt_kg_mean, total_bio_wwt_kg_var, total_bio_wwt_kg_sd,
                        pop_mean_no_m3, pop_var_no_m3, pop_sd_no_m3,pop_CI95_no_m3, total_indiv_mean, total_indiv_var,total_indiv_sd, n_obs)
      }

    } else {

      bio_mean_kg <- 0
      bio_var_kg <- NA
      bio_sd_kg <- NA
      bio_CI95_kg <- NA

      pop_mean_no_m3 <- 0
      pop_var_no_m3 <- NA
      pop_sd_no_m3 <- NA
      pop_CI95_no_m3 <- NA

      n_obs <- nrow(x)

      cell_area_m2 <- unique(x$area_m2)

      total_bio_wwt_kg_mean <- bio_mean_kg*area_m2
      total_bio_wwt_kg_var <- NA
      total_bio_wwt_kg_sd <- NA

      total_indiv_mean <- 0
      total_indiv_var <- NA
      total_indiv_sd <- NA

      mean_depth_m <- mean(x$depth_m)

      y <- data.frame(agg_grp, taxa, cell_id, decslat_cell_ctr, decslon_cell_ctr, mean_depth_m, area_m2, year, bio_mean_kg, bio_var_kg, bio_sd_kg, bio_CI95_kg,
                      total_bio_wwt_kg_mean, total_bio_wwt_kg_var, total_bio_wwt_kg_sd,
                      pop_mean_no_m3, pop_var_no_m3, pop_sd_no_m3,pop_CI95_no_m3, total_indiv_mean, total_indiv_var,total_indiv_sd, n_obs)

    }

  }

  return(y)

}, .progress = "text", .inform = T)

j <- j[,c("year", "taxa","cell_id", "decslat_cell_ctr", "decslon_cell_ctr", "pop_mean_no_m3", "pop_var_no_m3","n_obs")]

write.csv(x = j, file = "Aurelia_SEAMAP_2012-2018_30minCell.csv", row.names = F)


#calculate annual mean biomass & population density | STEP 2 in JMP workflow | STEP 3 in JMP workflow


# BY SEASON & YEAR : Whole Gulf of Mexico time-series ------

yr_season_cell_stats <- ddply(m, .variables = c("cell_id","season", "year"), function(x){

  year <- unique(x$year)
  cell_id <- unique(x$cell_id)
  agg_grp <- unique(x$agg_grp)
  area_m2 <- unique(x$area_m2)
  decslat_cell_ctr <- unique(x$decslat_cell_ctr)
  decslon_cell_ctr <- unique(x$decslon_cell_ctr)
  n_obs <- nrow(x)
  season <- unique(x$season)

  if(n_obs>0){

    if(sum(x$biomass_den_kg_m3)>0){

      cell_area_m2 <- unique(x$area_m2)
      use_pop_den_no_m2 <- x$use_pop_den_no_m2

      if(n_obs>1){

        bio_stat <- f_delta_stat(x = x$biomass_den_kg_m3)
        bio_mean_kg <- bio_stat$mean
        bio_var_kg <- bio_stat$variance
        bio_sd_kg <- sqrt(bio_var_kg)
        bio_CI95_kg <- bio_stat$CI95

        pop_stat <- f_delta_stat(x = x$pop_den_no_m3)
        pop_mean_no_m3 <- pop_stat$mean
        pop_var_no_m3 <- pop_stat$variance
        pop_sd_no_m3 <- sqrt(pop_var_no_m3)
        pop_CI95_no_m3 <- pop_stat$CI95

        total_bio_wwt_kg_mean <- bio_mean_kg*cell_area_m2
        total_bio_wwt_kg_var <- bio_var_kg*cell_area_m2
        total_bio_wwt_kg_sd <- sqrt(total_bio_wwt_kg_var)

        total_indiv_mean <- mean(use_pop_den_no_m2)*cell_area_m2
        total_indiv_var <- var(use_pop_den_no_m2)*cell_area_m2
        total_indiv_sd <- sqrt(total_indiv_var)

        mean_depth_m <- mean(x$depth_m)

        y <- data.frame(year, season, agg_grp, cell_id, decslat_cell_ctr, decslon_cell_ctr, mean_depth_m, area_m2, bio_mean_kg, bio_var_kg, bio_sd_kg, bio_CI95_kg,
                        total_bio_wwt_kg_mean, total_bio_wwt_kg_var, total_bio_wwt_kg_sd,
                        pop_mean_no_m3, pop_var_no_m3, pop_sd_no_m3,pop_CI95_no_m3, total_indiv_mean, total_indiv_var,total_indiv_sd, n_obs)

      } else {

        bio_stat <- f_delta_stat(x = x$biomass_den_kg_m3)
        bio_mean_kg <- bio_stat$mean
        bio_var_kg <- NA
        bio_sd_kg <- NA
        bio_CI95_kg <- NA

        pop_stat <- f_delta_stat(x = x$pop_den_no_m3)
        pop_mean_no_m3 <- pop_stat$mean
        pop_var_no_m3 <- NA
        pop_sd_no_m3 <- NA
        pop_CI95_no_m3 <- NA

        n_obs <- nrow(x)

        total_bio_wwt_kg_mean <- bio_mean_kg*area_m2
        total_bio_wwt_kg_var <- NA
        total_bio_wwt_kg_sd <- NA

        total_indiv_mean <- mean(use_pop_den_no_m2)*cell_area_m2
        total_indiv_var <- NA
        total_indiv_sd <- NA

        mean_depth_m <- mean(x$depth_m)

        y <- data.frame(year, season, agg_grp, cell_id, decslat_cell_ctr, decslon_cell_ctr, mean_depth_m, area_m2, bio_mean_kg, bio_var_kg, bio_sd_kg, bio_CI95_kg,
                        total_bio_wwt_kg_mean, total_bio_wwt_kg_var, total_bio_wwt_kg_sd,
                        pop_mean_no_m3, pop_var_no_m3, pop_sd_no_m3,pop_CI95_no_m3, total_indiv_mean, total_indiv_var,total_indiv_sd, n_obs)
      }

    } else {

      bio_mean_kg <- 0
      bio_var_kg <- NA
      bio_sd_kg <- NA
      bio_CI95_kg <- NA

      pop_mean_no_m3 <- 0
      pop_var_no_m3 <- NA
      pop_sd_no_m3 <- NA
      pop_CI95_no_m3 <- NA

      n_obs <- nrow(x)

      cell_area_m2 <- unique(x$area_m2)

      total_bio_wwt_kg_mean <- bio_mean_kg*area_m2
      total_bio_wwt_kg_var <- NA
      total_bio_wwt_kg_sd <- NA

      total_indiv_mean <- 0
      total_indiv_var <- NA
      total_indiv_sd <- NA

      mean_depth_m <- mean(x$depth_m)

      y <- data.frame(year, season, agg_grp, cell_id, decslat_cell_ctr, decslon_cell_ctr, mean_depth_m, area_m2, bio_mean_kg, bio_var_kg, bio_sd_kg, bio_CI95_kg,
                      total_bio_wwt_kg_mean, total_bio_wwt_kg_var, total_bio_wwt_kg_sd,
                      pop_mean_no_m3, pop_var_no_m3, pop_sd_no_m3,pop_CI95_no_m3, total_indiv_mean, total_indiv_var,total_indiv_sd, n_obs)

    }

  }

  return(y)

}, .progress = "text", .inform = T)

yr_season_cell_stats$subregion_alongshore = with(yr_season_cell_stats, ifelse(decslon_cell_ctr < -94, "1_Tex",
                                                                ifelse(decslon_cell_ctr > -88, "3_Fla", "2_Lou")))

yr_season_cell_stats$subregion_depth = with(yr_season_cell_stats,
                                     ifelse(mean_depth_m <= 20, "1_inshore",ifelse(is.na(mean_depth_m), NA,
                                                                                   ifelse(mean_depth_m <= 200, "2_shelf", "3_oceanic"))))

yr_season_cell_stats$agg_grp <- as.character(yr_season_cell_stats$agg_grp)


# GET YEAR statistics for each season
yr_season_stats <- ddply(yr_season_cell_stats, .variables = c("year","season"), function(x){

  year <- unique(x$year)
  agg_grp <- unique(x$agg_grp)
  season <- unique(x$season)

  n_cells <- length(unique(x$cell_id))

  if(n_cells>0){

    if(sum(x$total_bio_wwt_kg_mea)>0){

      n_stns <- sum(x$n_obs)

      bio_stat <- f_delta_stat(x = x$total_bio_wwt_kg_mean)
      bio_delta_mean_kg <- bio_stat$mean
      bio_delta_var_kg <- bio_stat$variance
      bio_delta_CI95_kg <- bio_stat$CI95
      bio_delta_sd_kg <- sqrt(bio_delta_var_kg)

      pop_stat <- f_delta_stat(x = x$pop_mean_no_m3)
      pop_delta_mean_no_m3 <- pop_stat$mean
      pop_delta_var_no_m3 <- pop_stat$variance
      pop_delta_CI95_no_m3 <- pop_stat$CI95
      pop_delta_sd_no_m3 <- sqrt(pop_delta_var_no_m3)

      pop_wt_mean_no_m3 <- wtd.mean(x$pop_mean_no_m3, weights = x$n_obs)

      total_stat <- f_delta_stat(x = x$total_indiv_mean)
      total_indiv_mean <- total_stat$mean
      total_indiv_var <- total_stat$variance
      total_indiv_CI95 <- total_stat$CI95
      total_indiv_sd <- sqrt(total_indiv_var)

      y <- data.frame(year, season, n_cells, n_stns, agg_grp, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_CI95_kg, bio_delta_sd_kg,
                      pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_CI95_no_m3, pop_delta_sd_no_m3, pop_wt_mean_no_m3, total_indiv_mean,
                      total_indiv_var, total_indiv_CI95, total_indiv_sd)

    } else {

      n_stns <- sum(x$n_obs)

      bio_delta_mean_kg <- 0
      bio_delta_var_kg <-0
      bio_delta_sd_kg <- sqrt(bio_delta_var_kg)
      bio_delta_CI95_kg <- NA

      pop_delta_mean_no_m3 <- 0
      pop_delta_var_no_m3 <-  NA
      pop_delta_sd_no_m3 <- sqrt(pop_delta_var_no_m3)
      pop_delta_CI95_no_m3 <- NA

      pop_wt_mean_no_m3 <- 0

      total_indiv_mean <- 0
      total_indiv_var <- NA
      total_indiv_sd <- NA
      total_indiv_CI95 <- NA

      y <- data.frame(year, season, n_cells, n_stns, agg_grp, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_CI95_kg, bio_delta_sd_kg,
                      pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_CI95_no_m3, pop_delta_sd_no_m3, pop_wt_mean_no_m3, total_indiv_mean,
                      total_indiv_var, total_indiv_CI95, total_indiv_sd)
    }

  }

  return(y)

}, .progress = "text", .inform = T)
  #assign decades
  f_get_decade <- function(x){

  m <- as.numeric(getElement(x, "year"))

  if(m >=1982 & m < 1992){
    decade <- "1982-1991"
  } else if(m >= 1992 & m < 2002){
    decade <- "1992-2001"
  } else if(m >= 2002 & m < 2012){
    decade <- "2002-2011"
  } else {
    decade <- "2012-2018"
  }
  return(decade)
  rm(m)
}
  yr_season_stats$decade <- apply(X = yr_season_stats, MARGIN = 1, FUN = f_get_decade)

  #make bar graphs
  g <- ggplot(data = yr_season_stats, aes(y=pop_delta_var_no_m3, x = decade, fill = season)) + geom_bar(stat = "identity", position=position_dodge())
  g <- ggplot(data = yr_season_stats, aes(y=pop_delta_mean_no_m3, x = decade, fill = season)) + geom_bar(stat = "identity", position=position_dodge())

  yr_season_stats$log_delta_mean <- log(yr_season_stats$pop_delta_mean_no_m3+1)
  yr_season_stats$log_delta_sd <- log(yr_season_stats$pop_delta_sd_no_m3+1)
  yr_season_stats$log_delta_CI95 <- log(yr_season_stats$pop_delta_CI95_no_m3+1)
  yr_season_stats$log_delta_se <- log((yr_season_stats$pop_delta_sd_no_m3/sqrt(yr_season_stats$n_cells))+1)


  g <- ggplot(data = yr_season_stats, aes(y=log_delta_mean, x = year, colour = season)) + geom_point() + geom_line() +
    scale_x_continuous(name = NULL, breaks = seq(1982,2018,2)) +
    scale_y_continuous(expand = c(0,0)) +
    geom_ribbon(data = yr_season_stats, aes(x=year, ymin = (log_delta_mean - log_delta_CI95), ymax = (log_delta_mean + log_delta_CI95)), fill = "palegreen4", alpha = 0.5)

    fall <- yr_season_stats[yr_season_stats$season == "fall",]
    fallrun3yr <- frollmean(x = fall$log_delta_mean, n = 3, fill = 0, align = "right")

    g <- ggplot(data = fall, aes(y=log_delta_mean, x = year)) +
      geom_ribbon(data = fall, aes(x=year, ymin = (log_delta_mean - log_delta_sd), ymax = (log_delta_mean + log_delta_sd)), fill = "palegreen4", alpha = 0.5) +
      geom_point(size = 2) + geom_line(size = 1) + geom_line(aes(y=fallrun3yr), size = 1.5, colour = "darkblue") +
      scale_x_continuous(name = NULL, breaks = seq(1982,2018,1)) +
      scale_y_continuous(name = "ln(mean individuals/m3 + 1) ± 1 sd", expand = c(0,0), breaks = seq(0,6,0.5)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 14)) +
      ggtitle(label = "Whole nGoMex: Fall Aurelia spp. Abundance")

    file = "Aurelia_Fall_Whole-GoMex.png"
    png(file = file, width = 12, height = 8, units = "in", res = 300)
    plot(g)
    dev.off()




# BY YEAR : Whole Gulf of Mexico time-series ------
yr_stats <- ddply(yr_cell_stats, .variables = c("year"), function(x){

  year <- unique(x$year)
  agg_grp <- unique(x$agg_grp)

  n_cells <- length(unique(x$cell_id))

 if(n_cells>0){

    if(sum(x$total_bio_wwt_kg_mea)>0){

      n_stns <- sum(x$n_obs)

      bio_stat <- f_delta_stat(x = x$total_bio_wwt_kg_mean)
      bio_delta_mean_kg <- bio_stat$mean
      bio_delta_var_kg <- bio_stat$variance
      bio_delta_CI95_kg <- bio_stat$CI95
      bio_delta_sd_kg <- sqrt(bio_delta_var_kg)

      pop_stat <- f_delta_stat(x = x$pop_mean_no_m3)
      pop_delta_mean_no_m3 <- pop_stat$mean
      pop_delta_var_no_m3 <- pop_stat$variance
      pop_delta_CI95_no_m3 <- pop_stat$CI95
      pop_delta_sd_no_m3 <- sqrt(pop_delta_var_no_m3)

      pop_wt_mean_no_m3 <- wtd.mean(x$pop_mean_no_m3, weights = x$n_obs)

      total_stat <- f_delta_stat(x = x$total_indiv_mean)
      total_indiv_mean <- total_stat$mean
      total_indiv_var <- total_stat$variance
      total_indiv_CI95 <- total_stat$CI95
      total_indiv_sd <- sqrt(total_indiv_var)

      y <- data.frame(year, n_cells, n_stns, agg_grp, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_CI95_kg, bio_delta_sd_kg,
                      pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_CI95_no_m3, pop_delta_sd_no_m3, pop_wt_mean_no_m3, total_indiv_mean,
                      total_indiv_var, total_indiv_CI95, total_indiv_sd)

    } else {

      n_stns <- sum(x$n_obs)

      bio_delta_mean_kg <- 0
      bio_delta_var_kg <-0
      bio_delta_sd_kg <- sqrt(bio_delta_var_kg)
      bio_delta_CI95_kg <- NA

      pop_delta_mean_no_m3 <- 0
      pop_delta_var_no_m3 <-  NA
      pop_delta_sd_no_m3 <- sqrt(pop_delta_var_no_m3)
      pop_delta_CI95_no_m3 <- NA

      pop_wt_mean_no_m3 <- 0

      total_indiv_mean <- 0
      total_indiv_var <- NA
      total_indiv_sd <- NA
      total_indiv_CI95 <- NA

      y <- data.frame(year, n_cells, n_stns, agg_grp, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_CI95_kg, bio_delta_sd_kg,
                      pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_CI95_no_m3, pop_delta_sd_no_m3, pop_wt_mean_no_m3, total_indiv_mean,
                      total_indiv_var, total_indiv_CI95, total_indiv_sd)
    }

  }

  return(y)

}, .progress = "text", .inform = T)

f_get_decade <- function(x){

  m <- as.numeric(getElement(x, "year"))

  if(m >=1982 & m < 1992){
    decade <- "1982-1991"
  } else if(m >= 1992 & m < 2002){
    decade <- "1992-2001"
  } else if(m >= 2002 & m < 2012){
    decade <- "2002-2011"
  } else {
    decade <- "2012-2018"
  }
  return(decade)
  rm(m)
}

yr_stats$decade <- apply(X = yr_stats, MARGIN = 1, FUN = f_get_decade)

g <- ggplot(data = yr_stats, aes(y=pop_delta_var_no_m3, x = decade)) + geom_bar(stat = "identity")
g <- ggplot(data = yr_stats, aes(y=pop_delta_mean_no_m3, x = decade)) + geom_bar(stat = "identity")

gp <- plot(x=yr_stats$year, y=log10(yr_stats$pop_wt_mean_no_m3+1), type = "b")

yr_stats$log_delta_mean <- log(yr_stats$pop_delta_mean_no_m3+1)
yr_stats$log_delta_sd <- log(yr_stats$pop_delta_sd_no_m3+1)
yr_stats$log_delta_CI95 <- log(yr_stats$pop_delta_CI95_no_m3+1)
yr_stats$log_delta_se <- log((yr_stats$pop_delta_sd_no_m3/sqrt(yr_stats$n_cells))+1)


g <- ggplot(data = yr_stats) + geom_point(aes(y=log_delta_mean, x = year)) + geom_line(aes(y=log_delta_mean, x = year)) +
  scale_x_continuous(name = NULL, breaks = seq(1984,2018,2)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_ribbon(data = yr_stats, aes(x=year, ymin = (log_delta_mean - log_delta_CI95), ymax = (log_delta_mean + log_delta_CI95)), fill = "palegreen4", alpha = 0.5)

p <- plot(x=yr_stats$year, y=yr_stats$pop_delta_mean_no_m3, type = "b")
p <- plot(x=yr_stats$year, y=yr_stats$bio_delta_mean_kg, type = "b")

model <- lm(data = yr_stats, formula = year ~ log(pop_delta_mean_no_m3+1))
summary(model)

save(yr_stats, file = "Aurelia_SEAMAP_All_GoM_time-series.Rdata")


# By Subregion_alongshore time-series ------
yr_shore_stats <- ddply(yr_cell_stats, .variables = c("year","subregion_alongshore"), function(x){

  agg_grp <- unique(x$agg_grp)

  if(nrow(x)>0){

    if(sum(x$total_bio_wwt_kg_mea)>0){

      bio_stat <- f_delta_stat(x = x$total_bio_wwt_kg_mean)
      bio_delta_mean_kg <- bio_stat$mean
      bio_delta_var_kg <- bio_stat$variance
      bio_delta_sd_kg <- sqrt(bio_delta_var_kg)

      pop_stat <- f_delta_stat(x = x$pop_mean_no_m3)
      pop_delta_mean_no_m3 <- pop_stat$mean
      pop_delta_var_no_m3 <-  pop_stat$variance
      pop_delta_sd_no_m3 <- sqrt(pop_delta_var_no_m3)

      pop_wt_mean_no_m3 <- wtd.mean(x$pop_mean_no_m3, weights = x$n_obs)

      n_obs <- nrow(x)

      y <- data.frame(agg_grp, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_sd_kg,
                      pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_sd_no_m3, pop_wt_mean_no_m3, n_obs)

    } else {

      bio_delta_mean_kg <- 0
      bio_delta_var_kg <-0
      bio_delta_sd_kg <- 0

      pop_delta_mean_no_m3 <- 0
      pop_delta_var_no_m3 <-  0
      pop_delta_sd_no_m3 <- 0

      pop_wt_mean_no_m3 <- 0

      n_obs <- nrow(x)

      y <- data.frame(agg_grp, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_sd_kg,
                      pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_sd_no_m3, pop_wt_mean_no_m3, n_obs)

    }

  }

  return(y)

}, .progress = "text", .inform = T)

save(yr_shore_stats, file = "Aurelia_SEAMAP_GoM_region_time-series.Rdata")


  # TEXAS
  tx <- yr_shore_stats[yr_shore_stats$subregion_alongshore == "1_Tex",]

  p <- plot(x=tx$year, y=tx$pop_delta_mean_no_m3, type = "b")
  p <- plot(x=tx$year, y=tx$bio_delta_mean_kg, type = "b")

  model.tx <- lm(data = tx, formula = year ~ log(pop_delta_mean_no_m3+1))
  summary(model.tx) #Not significant change over time

  # LOUISIANA
  la <- yr_shore_stats[yr_shore_stats$subregion_alongshore == "2_Lou",]

  p <- plot(x=la$year, y=log(la$pop_delta_mean_no_m3+1), type = "b")
  p <- plot(x=la$year, y=la$pop_wt_mean_no_m3, type = "b")
  p <- plot(x=la$year, y=la$bio_delta_mean_kg, type = "b")

  model.la <- lm(data = la, formula = year ~ log(pop_delta_mean_no_m3+1))
  summary(model.la) #Not significant change over time

  # FLORIDA
  fl <- yr_shore_stats[yr_shore_stats$subregion_alongshore == "3_Fla",]

  p <- plot(x=fl$year, y=log(fl$pop_wt_mean_no_m3+1), type = "b")
  p <- plot(x=fl$year, y=log(fl$pop_delta_mean_no_m3+1), type = "b")

  p <- plot(x=fl$year, y=fl$n_obs, type = "b")

  fl.nfive <- fl[fl$n_obs > 5,]

  p <- plot(x=fl.nfive$year, y=log(fl.nfive$pop_delta_mean_no_m3+1), type = "b")
  p <- plot(x=fl.nfive$year, y=fl.nfive$n_obs, type = "b")

  model.fl <- lm(data = fl.nfive, formula = year ~ log(pop_delta_mean_no_m3+1))
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


p <-  ggplot(data=y, aes(x=year, y=log(pop_wt_mean_no_m3+1))) + geom_line() + geom_point()


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
  tx.shelf <- yr_shore_depth_stats[yr_shore_depth_stats$subregion_depth == "2_shelf" & yr_shore_depth_stats$subregion_alongshore == "1_Tex",]

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
  la.shelf <- yr_shore_depth_stats[yr_shore_depth_stats$subregion_depth == "2_shelf" & yr_shore_depth_stats$subregion_alongshore == "2_Lou",]

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

  # FL - SHELF
  fl.shelf <- yr_shore_depth_stats[yr_shore_depth_stats$subregion_depth == "2_shelf" & yr_shore_depth_stats$subregion_alongshore == "3_Fla",]

  p <- plot(x=fl.shelf$year, y=log(fl.shelf$pop_wt_mean_no_m3+1), type = "b")
  p <- plot(x=fl.shelf$year, y=fl.shelf$n_obs, type = "b")

  model.fl.shelf <- lm(data = fl.shelf, formula = year ~ log(pop_wt_mean_no_m3+1))
  summary(model.fl.shelf) #Not significant change over time, p = 0.111



#CHYRSAORA ---------------

  if(cell_area == 15){
    d <- as.data.frame(fread(input = "chrysaora_15minCell_statareas.txt", sep = ",", stringsAsFactors = F))
  } else {
    warning('no input taxa data')
  }

  names(d) <- tolower(names(d))

  #d <- d[complete.cases(d),] #remove the 3 cases of NAs

  d <- d[,c(4:length((d)))]
  d <- plyr::rename(d, c("join_fid" = "cell_id"))

  f_get_season <- function(x){

    m <- as.numeric(getElement(x, "month"))

    if(m > 11 & m < 3){
      season <- "winter"
    } else if(m > 9 & m < 11){
      season <- "fall"
    } else if(m > 4 & m < 9){
      season <- "summer"
    } else {
      season <- "soring"
    }
    return(season)
    rm(m)
  }
  d$season <- apply(X = d, MARGIN = 1, FUN = f_get_season)

  #subset summery & fall seasons
  d <- d[d$season == "fall" | d$season == "summer",]

  #Read in Cell Areas -----
  if(cell_area == 15){
    a<- as.data.frame(fread(input = "15minCellArea.txt", sep = ",", stringsAsFactors = F))
  } else if(cell_area == 30){
    a<- as.data.frame(fread(input = "15minCellArea.txt", sep = ",", stringsAsFactors = F))
  }

  names(a) <- tolower(names(a))

  a <- a[,c(1,3,5:6)]
  a <- plyr::rename(a, c("objectid" = "cell_id"))
  a <- plyr::rename(a, c("longitude" = "decslon_cell_ctr"))
  a <- plyr::rename(a, c("latitude" = "decslat_cell_ctr"))


  m <- merge(d, a ,by = "cell_id")
  m <- plyr::rename(m, c("shape_area" = "area_m2"))

  #calculate annual mean biomass & population density in each fish net cell ; STEP 2 in JMP workflow -----------
  yr_cell_stats <- ddply(m, .variables = c("cell_id","year"), function(x){

    year <- unique(x$year)
    cell_id <- unique(x$cell_id)
    agg_grp <- unique(x$agg_grp)
    area_m2 <- unique(x$area_m2)
    decslat_cell_ctr <- unique(x$decslat_cell_ctr)
    decslon_cell_ctr <- unique(x$decslon_cell_ctr)
    n_obs <- nrow(x)

    if(n_obs>0){

      if(sum(x$biomass_den_kg_m3)>0){

        cell_area_m2 <- unique(x$area_m2)
        use_pop_den_no_m2 <- x$use_pop_den_no_m2

        if(n_obs>1){

          bio_stat <- f_delta_stat(x = x$biomass_den_kg_m3)
          bio_mean_kg <- bio_stat$mean
          bio_var_kg <- bio_stat$variance
          bio_sd_kg <- sqrt(bio_var_kg)
          bio_CI95_kg <- bio_stat$CI95

          pop_stat <- f_delta_stat(x = x$pop_den_no_m3)
          pop_mean_no_m3 <- pop_stat$mean
          pop_var_no_m3 <- pop_stat$variance
          pop_sd_no_m3 <- sqrt(pop_var_no_m3)
          pop_CI95_no_m3 <- pop_stat$CI95

          total_bio_wwt_kg_mean <- bio_mean_kg*cell_area_m2
          total_bio_wwt_kg_var <- bio_var_kg*cell_area_m2
          total_bio_wwt_kg_sd <- sqrt(total_bio_wwt_kg_var)

          total_indiv_mean <- mean(use_pop_den_no_m2)*cell_area_m2
          total_indiv_var <- var(use_pop_den_no_m2)*cell_area_m2
          total_indiv_sd <- sqrt(total_indiv_var)

          mean_depth_m <- mean(x$depth_m)

          y <- data.frame(cell_id, decslat_cell_ctr, decslon_cell_ctr, mean_depth_m, area_m2, year, agg_grp, bio_mean_kg, bio_var_kg, bio_sd_kg, bio_CI95_kg,
                          total_bio_wwt_kg_mean, total_bio_wwt_kg_var, total_bio_wwt_kg_sd,
                          pop_mean_no_m3, pop_var_no_m3, pop_sd_no_m3,pop_CI95_no_m3, total_indiv_mean, total_indiv_var,total_indiv_sd, n_obs)

        } else {

          bio_stat <- f_delta_stat(x = x$biomass_den_kg_m3)
          bio_mean_kg <- bio_stat$mean
          bio_var_kg <- NA
          bio_sd_kg <- NA
          bio_CI95_kg <- NA

          pop_stat <- f_delta_stat(x = x$pop_den_no_m3)
          pop_mean_no_m3 <- pop_stat$mean
          pop_var_no_m3 <- NA
          pop_sd_no_m3 <- NA
          pop_CI95_no_m3 <- NA

          n_obs <- nrow(x)

          total_bio_wwt_kg_mean <- bio_mean_kg*area_m2
          total_bio_wwt_kg_var <- NA
          total_bio_wwt_kg_sd <- NA

          total_indiv_mean <- mean(use_pop_den_no_m2)*cell_area_m2
          total_indiv_var <- NA
          total_indiv_sd <- NA

          mean_depth_m <- mean(x$depth_m)

          y <- data.frame(cell_id, decslat_cell_ctr, decslon_cell_ctr, mean_depth_m, area_m2, year, agg_grp, bio_mean_kg, bio_var_kg, bio_sd_kg, bio_CI95_kg,
                          total_bio_wwt_kg_mean, total_bio_wwt_kg_var, total_bio_wwt_kg_sd,
                          pop_mean_no_m3, pop_var_no_m3, pop_sd_no_m3,pop_CI95_no_m3, total_indiv_mean, total_indiv_var,total_indiv_sd, n_obs)
        }

      } else {

        bio_mean_kg <- 0
        bio_var_kg <- NA
        bio_sd_kg <- NA
        bio_CI95_kg <- NA

        pop_mean_no_m3 <- 0
        pop_var_no_m3 <- NA
        pop_sd_no_m3 <- NA
        pop_CI95_no_m3 <- NA

        n_obs <- nrow(x)

        cell_area_m2 <- unique(x$area_m2)

        total_bio_wwt_kg_mean <- bio_mean_kg*area_m2
        total_bio_wwt_kg_var <- NA
        total_bio_wwt_kg_sd <- NA

        total_indiv_mean <- 0
        total_indiv_var <- NA
        total_indiv_sd <- NA

        mean_depth_m <- mean(x$depth_m)

        y <- data.frame(cell_id, decslat_cell_ctr, decslon_cell_ctr, mean_depth_m, area_m2, year, agg_grp, bio_mean_kg, bio_var_kg, bio_sd_kg, bio_CI95_kg,
                        total_bio_wwt_kg_mean, total_bio_wwt_kg_var, total_bio_wwt_kg_sd,
                        pop_mean_no_m3, pop_var_no_m3, pop_sd_no_m3,pop_CI95_no_m3, total_indiv_mean, total_indiv_var,total_indiv_sd, n_obs)

      }

    }

    return(y)

  }, .progress = "text", .inform = T)

  yr_cell_stats$subregion_alongshore = with(yr_cell_stats, ifelse(decslon_cell_ctr < -94, "1_Tex",
                                                                  ifelse(decslon_cell_ctr > -88, "3_Fla", "2_Lou")))

  yr_cell_stats$subregion_depth = with(yr_cell_stats,
                                       ifelse(mean_depth_m <= 20, "1_inshore",ifelse(is.na(mean_depth_m), NA,
                                                                                     ifelse(mean_depth_m <= 200, "2_shelf", "3_oceanic"))))

  yr_cell_stats$agg_grp <- as.character(yr_cell_stats$agg_grp)

  # #data subsetted to send to Dr. Hui Lui @ TAMU-CC
  # j <- yr_cell_stats[yr_cell_stats$year > 2011,]
  # j <- plyr::rename(j, c("agg_grp" = "taxa_grp"))
  # j$taxa_grp <- "Aurelia_spp"
  # j <- j[,c("cell_id", "decslat_cell_ctr", "decslon_cell_ctr","year","taxa_grp","pop_mean_no_m3", "pop_var_no_m3","n_obs")]
  #
  # write.csv(x = j, file = "Aurliea_GoMex_SEAMAP_2011-2017_30minCells.csv", row.names = F)


  #calculate annual mean biomass & population density | STEP 2 in JMP workflow | STEP 3 in JMP workflow


  # BY SEASON & YEAR : Whole Gulf of Mexico time-series ------

  yr_season_cell_stats <- ddply(m, .variables = c("cell_id","season", "year"), function(x){

    year <- unique(x$year)
    cell_id <- unique(x$cell_id)
    agg_grp <- unique(x$agg_grp)
    area_m2 <- unique(x$area_m2)
    decslat_cell_ctr <- unique(x$decslat_cell_ctr)
    decslon_cell_ctr <- unique(x$decslon_cell_ctr)
    n_obs <- nrow(x)
    season <- unique(x$season)

    if(n_obs>0){

      if(sum(x$biomass_den_kg_m3)>0){

        cell_area_m2 <- unique(x$area_m2)
        use_pop_den_no_m2 <- x$use_pop_den_no_m2

        if(n_obs>1){

          bio_stat <- f_delta_stat(x = x$biomass_den_kg_m3)
          bio_mean_kg <- bio_stat$mean
          bio_var_kg <- bio_stat$variance
          bio_sd_kg <- sqrt(bio_var_kg)
          bio_CI95_kg <- bio_stat$CI95

          pop_stat <- f_delta_stat(x = x$pop_den_no_m3)
          pop_mean_no_m3 <- pop_stat$mean
          pop_var_no_m3 <- pop_stat$variance
          pop_sd_no_m3 <- sqrt(pop_var_no_m3)
          pop_CI95_no_m3 <- pop_stat$CI95

          total_bio_wwt_kg_mean <- bio_mean_kg*cell_area_m2
          total_bio_wwt_kg_var <- bio_var_kg*cell_area_m2
          total_bio_wwt_kg_sd <- sqrt(total_bio_wwt_kg_var)

          total_indiv_mean <- mean(use_pop_den_no_m2)*cell_area_m2
          total_indiv_var <- var(use_pop_den_no_m2)*cell_area_m2
          total_indiv_sd <- sqrt(total_indiv_var)

          mean_depth_m <- mean(x$depth_m)

          y <- data.frame(year, season, agg_grp, cell_id, decslat_cell_ctr, decslon_cell_ctr, mean_depth_m, area_m2, bio_mean_kg, bio_var_kg, bio_sd_kg, bio_CI95_kg,
                          total_bio_wwt_kg_mean, total_bio_wwt_kg_var, total_bio_wwt_kg_sd,
                          pop_mean_no_m3, pop_var_no_m3, pop_sd_no_m3,pop_CI95_no_m3, total_indiv_mean, total_indiv_var,total_indiv_sd, n_obs)

        } else {

          bio_stat <- f_delta_stat(x = x$biomass_den_kg_m3)
          bio_mean_kg <- bio_stat$mean
          bio_var_kg <- NA
          bio_sd_kg <- NA
          bio_CI95_kg <- NA

          pop_stat <- f_delta_stat(x = x$pop_den_no_m3)
          pop_mean_no_m3 <- pop_stat$mean
          pop_var_no_m3 <- NA
          pop_sd_no_m3 <- NA
          pop_CI95_no_m3 <- NA

          n_obs <- nrow(x)

          total_bio_wwt_kg_mean <- bio_mean_kg*area_m2
          total_bio_wwt_kg_var <- NA
          total_bio_wwt_kg_sd <- NA

          total_indiv_mean <- mean(use_pop_den_no_m2)*cell_area_m2
          total_indiv_var <- NA
          total_indiv_sd <- NA

          mean_depth_m <- mean(x$depth_m)

          y <- data.frame(year, season, agg_grp, cell_id, decslat_cell_ctr, decslon_cell_ctr, mean_depth_m, area_m2, bio_mean_kg, bio_var_kg, bio_sd_kg, bio_CI95_kg,
                          total_bio_wwt_kg_mean, total_bio_wwt_kg_var, total_bio_wwt_kg_sd,
                          pop_mean_no_m3, pop_var_no_m3, pop_sd_no_m3,pop_CI95_no_m3, total_indiv_mean, total_indiv_var,total_indiv_sd, n_obs)
        }

      } else {

        bio_mean_kg <- 0
        bio_var_kg <- NA
        bio_sd_kg <- NA
        bio_CI95_kg <- NA

        pop_mean_no_m3 <- 0
        pop_var_no_m3 <- NA
        pop_sd_no_m3 <- NA
        pop_CI95_no_m3 <- NA

        n_obs <- nrow(x)

        cell_area_m2 <- unique(x$area_m2)

        total_bio_wwt_kg_mean <- bio_mean_kg*area_m2
        total_bio_wwt_kg_var <- NA
        total_bio_wwt_kg_sd <- NA

        total_indiv_mean <- 0
        total_indiv_var <- NA
        total_indiv_sd <- NA

        mean_depth_m <- mean(x$depth_m)

        y <- data.frame(year, season, agg_grp, cell_id, decslat_cell_ctr, decslon_cell_ctr, mean_depth_m, area_m2, bio_mean_kg, bio_var_kg, bio_sd_kg, bio_CI95_kg,
                        total_bio_wwt_kg_mean, total_bio_wwt_kg_var, total_bio_wwt_kg_sd,
                        pop_mean_no_m3, pop_var_no_m3, pop_sd_no_m3,pop_CI95_no_m3, total_indiv_mean, total_indiv_var,total_indiv_sd, n_obs)

      }

    }

    return(y)

  }, .progress = "text", .inform = T)

  yr_season_cell_stats$subregion_alongshore = with(yr_season_cell_stats, ifelse(decslon_cell_ctr < -94, "1_Tex",
                                                                                ifelse(decslon_cell_ctr > -88, "3_Fla", "2_Lou")))

  yr_season_cell_stats$subregion_depth = with(yr_season_cell_stats,
                                              ifelse(mean_depth_m <= 20, "1_inshore",ifelse(is.na(mean_depth_m), NA,
                                                                                            ifelse(mean_depth_m <= 200, "2_shelf", "3_oceanic"))))

  yr_season_cell_stats$agg_grp <- as.character(yr_season_cell_stats$agg_grp)

  # GET YEAR statistics for each season
  yr_season_stats <- ddply(yr_season_cell_stats, .variables = c("year","season"), function(x){

    year <- unique(x$year)
    agg_grp <- unique(x$agg_grp)
    season <- unique(x$season)

    n_cells <- length(unique(x$cell_id))

    if(n_cells>0){

      if(sum(x$total_bio_wwt_kg_mea)>0){

        n_stns <- sum(x$n_obs)

        bio_stat <- f_delta_stat(x = x$total_bio_wwt_kg_mean)
        bio_delta_mean_kg <- bio_stat$mean
        bio_delta_var_kg <- bio_stat$variance
        bio_delta_CI95_kg <- bio_stat$CI95
        bio_delta_sd_kg <- sqrt(bio_delta_var_kg)

        pop_stat <- f_delta_stat(x = x$pop_mean_no_m3)
        pop_delta_mean_no_m3 <- pop_stat$mean
        pop_delta_var_no_m3 <- pop_stat$variance
        pop_delta_CI95_no_m3 <- pop_stat$CI95
        pop_delta_sd_no_m3 <- sqrt(pop_delta_var_no_m3)

        pop_wt_mean_no_m3 <- wtd.mean(x$pop_mean_no_m3, weights = x$n_obs)

        total_stat <- f_delta_stat(x = x$total_indiv_mean)
        total_indiv_mean <- total_stat$mean
        total_indiv_var <- total_stat$variance
        total_indiv_CI95 <- total_stat$CI95
        total_indiv_sd <- sqrt(total_indiv_var)

        y <- data.frame(year, season, n_cells, n_stns, agg_grp, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_CI95_kg, bio_delta_sd_kg,
                        pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_CI95_no_m3, pop_delta_sd_no_m3, pop_wt_mean_no_m3, total_indiv_mean,
                        total_indiv_var, total_indiv_CI95, total_indiv_sd)

      } else {

        n_stns <- sum(x$n_obs)

        bio_delta_mean_kg <- 0
        bio_delta_var_kg <-0
        bio_delta_sd_kg <- sqrt(bio_delta_var_kg)
        bio_delta_CI95_kg <- NA

        pop_delta_mean_no_m3 <- 0
        pop_delta_var_no_m3 <-  NA
        pop_delta_sd_no_m3 <- sqrt(pop_delta_var_no_m3)
        pop_delta_CI95_no_m3 <- NA

        pop_wt_mean_no_m3 <- 0

        total_indiv_mean <- 0
        total_indiv_var <- NA
        total_indiv_sd <- NA
        total_indiv_CI95 <- NA

        y <- data.frame(year, season, n_cells, n_stns, agg_grp, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_CI95_kg, bio_delta_sd_kg,
                        pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_CI95_no_m3, pop_delta_sd_no_m3, pop_wt_mean_no_m3, total_indiv_mean,
                        total_indiv_var, total_indiv_CI95, total_indiv_sd)
      }

    }

    return(y)

  }, .progress = "text", .inform = T)
  #assign decades
  f_get_decade <- function(x){

    m <- as.numeric(getElement(x, "year"))

    if(m >=1982 & m < 1992){
      decade <- "1982-1991"
    } else if(m >= 1992 & m < 2002){
      decade <- "1992-2001"
    } else if(m >= 2002 & m < 2012){
      decade <- "2002-2011"
    } else {
      decade <- "2012-2018"
    }
    return(decade)
    rm(m)
  }
  yr_season_stats$decade <- apply(X = yr_season_stats, MARGIN = 1, FUN = f_get_decade)

  #make bar graphs
  g <- ggplot(data = yr_season_stats, aes(y=pop_delta_var_no_m3, x = decade, fill = season)) + geom_bar(stat = "identity", position=position_dodge())
  g <- ggplot(data = yr_season_stats, aes(y=pop_delta_mean_no_m3, x = decade, fill = season)) + geom_bar(stat = "identity", position=position_dodge())

  yr_season_stats$log_delta_mean <- log(yr_season_stats$pop_delta_mean_no_m3+1)
  yr_season_stats$log_delta_sd <- log(yr_season_stats$pop_delta_sd_no_m3+1)
  yr_season_stats$log_delta_CI95 <- log(yr_season_stats$pop_delta_CI95_no_m3+1)
  yr_season_stats$log_delta_se <- log((yr_season_stats$pop_delta_sd_no_m3/sqrt(yr_season_stats$n_cells))+1)


  g <- ggplot(data = yr_season_stats, aes(y=log_delta_mean, x = year, colour = season)) + geom_point() + geom_line() +
    scale_x_continuous(name = NULL, breaks = seq(1982,2018,2)) +
    scale_y_continuous(expand = c(0,0)) +
    geom_ribbon(data = yr_season_stats, aes(x=year, ymin = (log_delta_mean - log_delta_CI95), ymax = (log_delta_mean + log_delta_CI95)), fill = "palegreen4", alpha = 0.5)

  fall <- yr_season_stats[yr_season_stats$season == "fall",]
  fallrun3yr <- frollmean(x = fall$log_delta_mean, n = 3, fill = 0, align = "right")

  g <- ggplot(data = fall, aes(y=log_delta_mean, x = year)) +
    geom_ribbon(data = fall, aes(x=year, ymin = (log_delta_mean - log_delta_sd), ymax = (log_delta_mean + log_delta_sd)), fill = "palegreen4", alpha = 0.5) +
    geom_point(size = 2) + geom_line(size = 1) + geom_line(aes(y=fallrun3yr), size = 1.5, colour = "darkblue") +
    scale_x_continuous(name = NULL, breaks = seq(1982,2018,1)) +
    scale_y_continuous(name = "ln(mean individuals/m3 + 1) ± 1 sd", expand = c(0,0), breaks = seq(0,6,0.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 14)) +
    ggtitle(label = "Whole nGoMex: Fall Aurelia spp. Abundance")

  file = "Aurelia_Fall_Whole-GoMex.png"
  png(file = file, width = 12, height = 8, units = "in", res = 300)
  plot(g)
  dev.off()




  # BY YEAR : Whole Gulf of Mexico time-series ------
  yr_stats <- ddply(yr_cell_stats, .variables = c("year"), function(x){

    year <- unique(x$year)
    agg_grp <- unique(x$agg_grp)

    n_cells <- length(unique(x$cell_id))

    if(n_cells>0){

      if(sum(x$total_bio_wwt_kg_mea)>0){

        n_stns <- sum(x$n_obs)

        bio_stat <- f_delta_stat(x = x$total_bio_wwt_kg_mean)
        bio_delta_mean_kg <- bio_stat$mean
        bio_delta_var_kg <- bio_stat$variance
        bio_delta_CI95_kg <- bio_stat$CI95
        bio_delta_sd_kg <- sqrt(bio_delta_var_kg)

        pop_stat <- f_delta_stat(x = x$pop_mean_no_m3)
        pop_delta_mean_no_m3 <- pop_stat$mean
        pop_delta_var_no_m3 <- pop_stat$variance
        pop_delta_CI95_no_m3 <- pop_stat$CI95
        pop_delta_sd_no_m3 <- sqrt(pop_delta_var_no_m3)

        pop_wt_mean_no_m3 <- wtd.mean(x$pop_mean_no_m3, weights = x$n_obs)

        total_stat <- f_delta_stat(x = x$total_indiv_mean)
        total_indiv_mean <- total_stat$mean
        total_indiv_var <- total_stat$variance
        total_indiv_CI95 <- total_stat$CI95
        total_indiv_sd <- sqrt(total_indiv_var)

        y <- data.frame(year, n_cells, n_stns, agg_grp, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_CI95_kg, bio_delta_sd_kg,
                        pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_CI95_no_m3, pop_delta_sd_no_m3, pop_wt_mean_no_m3, total_indiv_mean,
                        total_indiv_var, total_indiv_CI95, total_indiv_sd)

      } else {

        n_stns <- sum(x$n_obs)

        bio_delta_mean_kg <- 0
        bio_delta_var_kg <-0
        bio_delta_sd_kg <- sqrt(bio_delta_var_kg)
        bio_delta_CI95_kg <- NA

        pop_delta_mean_no_m3 <- 0
        pop_delta_var_no_m3 <-  NA
        pop_delta_sd_no_m3 <- sqrt(pop_delta_var_no_m3)
        pop_delta_CI95_no_m3 <- NA

        pop_wt_mean_no_m3 <- 0

        total_indiv_mean <- 0
        total_indiv_var <- NA
        total_indiv_sd <- NA
        total_indiv_CI95 <- NA

        y <- data.frame(year, n_cells, n_stns, agg_grp, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_CI95_kg, bio_delta_sd_kg,
                        pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_CI95_no_m3, pop_delta_sd_no_m3, pop_wt_mean_no_m3, total_indiv_mean,
                        total_indiv_var, total_indiv_CI95, total_indiv_sd)
      }

    }

    return(y)

  }, .progress = "text", .inform = T)

  f_get_decade <- function(x){

    m <- as.numeric(getElement(x, "year"))

    if(m >=1982 & m < 1992){
      decade <- "1982-1991"
    } else if(m >= 1992 & m < 2002){
      decade <- "1992-2001"
    } else if(m >= 2002 & m < 2012){
      decade <- "2002-2011"
    } else {
      decade <- "2012-2018"
    }
    return(decade)
    rm(m)
  }

  yr_stats$decade <- apply(X = yr_stats, MARGIN = 1, FUN = f_get_decade)

  g <- ggplot(data = yr_stats, aes(y=pop_delta_var_no_m3, x = decade)) + geom_bar(stat = "identity")
  g <- ggplot(data = yr_stats, aes(y=pop_delta_mean_no_m3, x = decade)) + geom_bar(stat = "identity")



  p <- plot(x=yr_stats$year, y=log10(yr_stats$pop_wt_mean_no_m3+1), type = "b")

  yr_stats$log_delta_mean <- log(yr_stats$pop_delta_mean_no_m3+1)
  yr_stats$log_delta_sd <- log(yr_stats$pop_delta_sd_no_m3+1)
  yr_stats$log_delta_CI95 <- log(yr_stats$pop_delta_CI95_no_m3+1)
  yr_stats$log_delta_se <- log((yr_stats$pop_delta_sd_no_m3/sqrt(yr_stats$n_cells))+1)


  g <- ggplot(data = yr_stats) + geom_point(aes(y=log_delta_mean, x = year)) + geom_line(aes(y=log_delta_mean, x = year)) +
    scale_x_continuous(name = NULL, breaks = seq(1984,2018,2)) +
    scale_y_continuous(expand = c(0,0)) +
    geom_ribbon(data = yr_stats, aes(x=year, ymin = (log_delta_mean - log_delta_CI95), ymax = (log_delta_mean + log_delta_CI95)), fill = "palegreen4", alpha = 0.5)

  p <- plot(x=yr_stats$year, y=yr_stats$pop_delta_mean_no_m3, type = "b")
  p <- plot(x=yr_stats$year, y=yr_stats$bio_delta_mean_kg, type = "b")

  model <- lm(data = yr_stats, formula = year ~ log(pop_delta_mean_no_m3+1))
  summary(model)

  save(yr_stats, file = "Aurelia_SEAMAP_All_GoM_time-series.Rdata")


  # By Subregion_alongshore time-series ------
  yr_shore_stats <- ddply(yr_cell_stats, .variables = c("year","subregion_alongshore"), function(x){

    agg_grp <- unique(x$agg_grp)

    if(nrow(x)>0){

      if(sum(x$total_bio_wwt_kg_mea)>0){

        bio_stat <- f_delta_stat(x = x$total_bio_wwt_kg_mean)
        bio_delta_mean_kg <- bio_stat$mean
        bio_delta_var_kg <- bio_stat$variance
        bio_delta_sd_kg <- sqrt(bio_delta_var_kg)

        pop_stat <- f_delta_stat(x = x$pop_mean_no_m3)
        pop_delta_mean_no_m3 <- pop_stat$mean
        pop_delta_var_no_m3 <-  pop_stat$variance
        pop_delta_sd_no_m3 <- sqrt(pop_delta_var_no_m3)

        pop_wt_mean_no_m3 <- wtd.mean(x$pop_mean_no_m3, weights = x$n_obs)

        n_obs <- nrow(x)

        y <- data.frame(agg_grp, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_sd_kg,
                        pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_sd_no_m3, pop_wt_mean_no_m3, n_obs)

      } else {

        bio_delta_mean_kg <- 0
        bio_delta_var_kg <-0
        bio_delta_sd_kg <- 0

        pop_delta_mean_no_m3 <- 0
        pop_delta_var_no_m3 <-  0
        pop_delta_sd_no_m3 <- 0

        pop_wt_mean_no_m3 <- 0

        n_obs <- nrow(x)

        y <- data.frame(agg_grp, bio_delta_mean_kg, bio_delta_var_kg, bio_delta_sd_kg,
                        pop_delta_mean_no_m3, pop_delta_var_no_m3, pop_delta_sd_no_m3, pop_wt_mean_no_m3, n_obs)

      }

    }

    return(y)

  }, .progress = "text", .inform = T)

  save(yr_shore_stats, file = "Aureli_SEAMAP_GoM_region_time-series.Rdata")
  a

  # TEXAS
  tx <- yr_shore_stats[yr_shore_stats$subregion_alongshore == "1_Tex",]

  p <- plot(x=tx$year, y=tx$pop_delta_mean_no_m3, type = "b")
  p <- plot(x=tx$year, y=tx$bio_delta_mean_kg, type = "b")

  model.tx <- lm(data = tx, formula = year ~ log(pop_delta_mean_no_m3+1))
  summary(model.tx) #Not significant change over time

  # LOUISIANA
  la <- yr_shore_stats[yr_shore_stats$subregion_alongshore == "2_Lou",]

  p <- plot(x=la$year, y=log(la$pop_delta_mean_no_m3+1), type = "b")
  p <- plot(x=la$year, y=la$pop_wt_mean_no_m3, type = "b")
  p <- plot(x=la$year, y=la$bio_delta_mean_kg, type = "b")

  model.la <- lm(data = la, formula = year ~ log(pop_delta_mean_no_m3+1))
  summary(model.la) #Not significant change over time

  # FLORIDA
  fl <- yr_shore_stats[yr_shore_stats$subregion_alongshore == "3_Fla",]

  p <- plot(x=fl$year, y=log(fl$pop_wt_mean_no_m3+1), type = "b")
  p <- plot(x=fl$year, y=log(fl$pop_delta_mean_no_m3+1), type = "b")

  p <- plot(x=fl$year, y=fl$n_obs, type = "b")

  fl.nfive <- fl[fl$n_obs > 5,]

  p <- plot(x=fl.nfive$year, y=log(fl.nfive$pop_delta_mean_no_m3+1), type = "b")
  p <- plot(x=fl.nfive$year, y=fl.nfive$n_obs, type = "b")

  model.fl <- lm(data = fl.nfive, formula = year ~ log(pop_delta_mean_no_m3+1))
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


  p <-  ggplot(data=y, aes(x=year, y=log(pop_wt_mean_no_m3+1))) + geom_line() + geom_point()


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
  tx.shelf <- yr_shore_depth_stats[yr_shore_depth_stats$subregion_depth == "2_shelf" & yr_shore_depth_stats$subregion_alongshore == "1_Tex",]

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
  la.shelf <- yr_shore_depth_stats[yr_shore_depth_stats$subregion_depth == "2_shelf" & yr_shore_depth_stats$subregion_alongshore == "2_Lou",]

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

  # FL - SHELF
  fl.shelf <- yr_shore_depth_stats[yr_shore_depth_stats$subregion_depth == "2_shelf" & yr_shore_depth_stats$subregion_alongshore == "3_Fla",]

  p <- plot(x=fl.shelf$year, y=log(fl.shelf$pop_wt_mean_no_m3+1), type = "b")
  p <- plot(x=fl.shelf$year, y=fl.shelf$n_obs, type = "b")

  model.fl.shelf <- lm(data = fl.shelf, formula = year ~ log(pop_wt_mean_no_m3+1))
  summary(model.fl.shelf) #Not significant change over time, p = 0.111

