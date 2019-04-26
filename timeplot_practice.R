# create time plot for whole gulf

library(ggplot2)
library (tidyverse)
library(ggThemeAssist)
library(data.table)
library(plyr)
library (dplyr)


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
