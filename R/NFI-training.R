
library(tidyverse)

plot_init    <- readxl::read_excel("data/NFI training/data-NFI-training.xlsx", sheet = "plot")
lf_init      <- readxl::read_excel("data/NFI training/data-NFI-training.xlsx", sheet = "land_feature")
subplot_init <- readxl::read_excel("data/NFI training/data-NFI-training.xlsx", sheet = "subplot")
tree_init    <- readxl::read_excel("data/NFI training/data-NFI-training.xlsx", sheet = "tree")

## Check data type
summary(tree_init)

## Check distance
tree <- tree_init %>%
  mutate(
    tree_total_height = as.numeric(tree_total_height),
    tree_distance     = as.numeric(tree_distance),
    tree_azimuth      = as.numeric(tree_azimuth), 
    tree_dbh          = as.numeric(tree_dbh),
    subplot_id = paste(plot_no, subplot_no, sep = "_"),
    tree_id = paste(plot_no, subplot_no, lf_no, tree_no, sep = "_"),
    tree_check_distance = if_else(tree_distance <= 12, "M", "L")
  ) %>%
  filter(!is.na(tree_distance), !is.na(tree_azimuth))

summary(tree)
## Tree DBH

ggplot(tree, aes(x = tree_no, y = tree_dbh)) +
  geom_point(aes(color = tree_check_distance, shape = tree_check_distance), size = 2) +
  geom_abline(aes(slope = 0, intercept = 30))


ggplot(tree, aes(x = tree_distance, y = tree_dbh)) +
  geom_point(aes(color = tree_check_distance, shape = tree_check_distance), size = 2) +
  geom_abline(aes(slope = 0, intercept = 30))

ggplot(tree, aes(x = tree_distance, y = tree_dbh)) +
  geom_label(aes(color = tree_check_distance, label = tree_id), size = 4) +
  geom_abline(aes(slope = 0, intercept = 30))


## Tree Height
tree_h <- tree %>%
  filter(!is.na(tree_total_height))

ggplot(tree_h, aes(x = tree_dbh, y = tree_total_height)) +
  geom_point(aes(color = plot_no, shape = subplot_no), size = 2)


## Tree position
ggplot(tree, aes(x = tree_distance, y = tree_azimuth)) +
  geom_text(aes(label = tree_azimuth, color = plot_no)) +
  scale_x_continuous(breaks = c(0, 12, 25)) +
  scale_y_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
  coord_polar(theta = "y")

ggplot(tree, aes(x = tree_distance, y = tree_azimuth)) +
  geom_point(aes(color = subplot_id, size = tree_dbh)) +
  scale_x_continuous(breaks = c(0, 12, 25)) +
  scale_y_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
  coord_polar(theta = "y") +
  theme_bw()

ggplot(tree, aes(x = tree_distance, y = tree_azimuth)) +
  geom_point(aes(color = plot_no, size = tree_dbh)) +
  scale_x_continuous(breaks = c(0, 12, 25)) +
  scale_y_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
  coord_polar(theta = "y") +
  theme_linedraw()

## Per subplot
tt <- tree %>% filter(subplot_id == "T1_C")

ggplot(tt, aes(x = tree_distance, y = tree_azimuth)) +
  geom_point(aes(color = tree_check_distance, size = tree_dbh)) +
  scale_x_continuous(breaks = c(0, 12, 25)) +
  scale_y_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
  coord_polar(theta = "y") +
  theme_bw() +
  labs(title = unique(tt$subplot_id))


tt <- tree %>% filter(subplot_id == "T1_E1")

ggplot(tt, aes(x = tree_distance, y = tree_azimuth)) +
  geom_point(aes(color = tree_check_distance, size = tree_dbh)) +
  scale_x_continuous(breaks = c(0, 12, 25)) +
  scale_y_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
  coord_polar(theta = "y") +
  theme_bw() +
  labs(title = unique(tt$subplot_id))



tt <- tree %>% filter(subplot_id == "T2_C")

ggplot(tt, aes(x = tree_distance, y = tree_azimuth)) +
  geom_point(aes(color = tree_check_distance, size = tree_dbh)) +
  scale_x_continuous(breaks = c(0, 12, 25)) +
  scale_y_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
  coord_polar(theta = "y") +
  theme_bw() +
  labs(title = unique(tt$subplot_id))



tt <- tree %>% filter(subplot_id == "T2_E1")

ggplot(tt, aes(x = tree_distance, y = tree_azimuth)) +
  geom_point(aes(color = tree_check_distance, size = tree_dbh)) +
  scale_x_continuous(breaks = c(0, 12, 25)) +
  scale_y_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
  coord_polar(theta = "y") +
  theme_bw() +
  labs(title = unique(tt$subplot_id))


tt <- tree %>% filter(subplot_id == "T2_E2")

ggplot(tt, aes(x = tree_distance, y = tree_azimuth)) +
  geom_point(aes(color = tree_check_distance, size = tree_dbh)) +
  scale_x_continuous(breaks = c(0, 12, 25)) +
  scale_y_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
  coord_polar(theta = "y") +
  theme_bw() +
  labs(title = unique(tt$subplot_id))
