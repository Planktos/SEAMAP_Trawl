#Practice code to help Aidyn with her graphs
#Stacy Calhoun
#
# Libraries ---------------------------------------------------------------


library(tidyverse)
library(scales)


# Plot Code ---------------------------------------------------------------


yr_stats$pop_se_kg = yr_stats$pop_sd_kg/sqrt(length(yr_stats$pop_sd_kg))
yr_stats$bio_se_kg = yr_stats$bio_sd_kg/sqrt(length(yr_stats$bio_sd_kg))

#sd = with(yr_stats, aes(ymin = pop_mean_no_m3 - pop_sd_kg + 1, ymax = pop_mean_no_m3 + pop_sd_kg + 1))

wpg = ggplot(yr_stats, aes(x = year, y = log(pop_mean_no_m3 + 1))) +
  geom_point() +
  geom_line() +
  geom_ribbon(with(yr_stats, aes(ymin = pop_mean_no_m3 - (pop_se_kg + 1), ymax = pop_mean_no_m3 + (pop_se_kg + 1)), fill = 'grey70')) +
  #geom_errorbar(aes(ymin=pop_mean_no_m3 - (pop_sd_kg + 1), ymax=pop_mean_no_m3 + (pop_sd_kg + 1)), width=.2,
  #              position=position_dodge(0.05))+
  #scale_y_continuous(trans= "log", breaks = ) +
  theme_classic()

plot(wpg)

yr_stats$bio_mean_kg = ifelse(yr_stats$year == 1985, NA, yr_stats$bio_mean_kg)

wpg = ggplot(subset(yr_stats, yr_stats$year >= 1984), aes(x = year, y = bio_mean_kg + 1)) +
  geom_point(na.rm = T) +
  geom_line(na.rm = T) +
  #geom_ribbon(with(yr_stats, aes(ymin = bio_mean_kg - bio_sd_kg + 1, ymax = bio_mean_kg + bio_sd_kg + 1)), fill = 'grey70') +
  #geom_errorbar(aes(ymin=pop_mean_no_m3 - pop_sd_kg + 1, ymax=pop_mean_no_m3 + pop_sd_kg + 1), width=.2, position=position_dodge(0.05))+
  scale_y_continuous(trans= "log") +
  scale_x_continuous(breaks=seq(1984, 2018, 2))+
  theme_classic()

plot(wpg)



