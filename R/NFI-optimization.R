
library(tidyverse)
library(ggrepel)


tt <- read_csv("data/NFI optimization/oNFI-results.csv")
tt

tt %>%
  filter(nest2_radius == 10) %>%
  filter(distance_multiplier == 2) %>%
  ggplot(aes(x = total_time, y = cv)) +
  geom_point(aes(
    color = nest1_radius, 
    shape = as.character(subplot_count),
    size = n_plot
    )) +
  #geom_label_repel(aes(label = nest1_radius)) +
  scale_color_viridis_c() +
  theme_bw()


tt %>%
  filter(
    nest1_radius == 20,
    nest2_radius == 10,
    distance_multiplier == 2,
    ) %>%
  ggplot(aes(x = total_time, y = cv)) +
  geom_point() +
  geom_label_repel(aes(label = subplot_count)) +
  theme_bw()
  
  