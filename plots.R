
library(tidyverse)
library(scales)

sd = with(yr_stats, aes(ymin = pop_mean_no_m3 - pop_sd_kg + 1, ymax = pop_mean_no_m3 + pop_sd_kg + 1))

wpg = ggplot(yr_stats, aes(x = year, y = pop_mean_no_m3 + 1)) +
  geom_point() +
  geom_line() +
  geom_ribbon(with(yr_stats, aes(ymin = pop_mean_no_m3 - pop_sd_kg + 1, ymax = pop_mean_no_m3 + pop_sd_kg + 1)), fill = 'grey70') +
  scale_y_continuous(trans= "log10") +
  theme_classic()

plot(wpg)
