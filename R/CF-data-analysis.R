## Metadata

## Community forestry data analysis
## GCF project: 
## REDD+ Readiness Support to Timor-Leste: Capacity and Institutional Development 
## April 2022


## Libraries ----

library(tidyverse)



## Data ----

plot_init <- read_csv("data/CF/plot.csv", show_col_types = FALSE)
tree_init <- read_csv("data/CF/tree.csv", show_col_types = FALSE)
sapling_init <- read_csv("data/CF/sapling.csv", show_col_types = FALSE)



## Data Preparations ----


## --- Prepare plot 
plot_init

plot <- plot_init %>%
  mutate(
    forest_type = case_when(
      site == "PGA" ~ "Dense Natural Forest",
      plot_id %in% c("PMT01", "PMT02", "PMT06") ~ "Dense Natural Forest",
      plot_id %in% c("PMT03", "PMT05", "PMT05") ~ "Dense Natural Forest",  
      plot_id %in% c("PSL01", "PSL02", "PSL03") ~ "Dense Natural Forest",
      plot_id %in% c("PSL04", "PSL05")          ~ "Dense Plantation Forest",
      )
    )

## --- Prepare sapling to merge with tree
sapling_init

sapling <- sapling_init %>% 
  mutate(
    sapling_id = paste0(sapling_id, "sa"),
    sapling_no = sapling_no * 100
    )

names(sapling) <- names(tree_init)

## --- Prepare tree
tree_init

tree <- tree_init %>%
  bind_rows(sapling) %>%
  mutate(
    tree_dbh_cor = round(tree_girth / pi, 0),
    tree_ba_cor  = round((tree_dbh / 200)^2 ),
    tree_volume  = round(tree_ba_cor * tree_height_top)
  ) %>%
  left_join(plot, by = "plot_id")



## Visual checks

tree %>%
  ggplot(aes(x = tree_dbh, y = tree_height_top)) +
  geom_point()


tree %>%
  ggplot(aes(x = tree_dbh, y = tree_height_top)) +
  geom_point(aes(color = site, shape = site)) +
  scale_shape_manual(values = c(1, 3, 2)) +
  theme_bw() +
  labs(x = "DBH (cm)", y = "Height (m)")




