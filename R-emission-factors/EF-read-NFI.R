
library(sf)
library(terra)
library(tidyverse)
library(readxl)

ggplot2::theme_set(ggplot2::theme_bw())



##
## Load data ################################################################
##

path_nfi_data <- "data/NFItest/NFI-test.xlsx"

readxl::excel_sheets(path_nfi_data)

tree_init      <- readxl::read_xlsx(path_nfi_data, sheet = "tree")
sapling_init   <- readxl::read_xlsx(path_nfi_data, sheet = "sapling")
plot_init      <- readxl::read_xlsx(path_nfi_data, sheet = "plot")
subplot_init   <- readxl::read_xlsx(path_nfi_data, sheet = "subplot")
lf_init        <- readxl::read_xlsx(path_nfi_data, sheet = "land_feature")
lf_object_init <- readxl::read_xlsx(path_nfi_data, sheet = "land_feature_object")
species_list   <- readxl::read_xlsx(path_nfi_data, sheet = "SPECIES_LIST")

gwd <- read_csv("data/gwd.csv")

gwd_species <- gwd %>% 
  group_by(Binomial) %>%
  summarise(wd_mean = mean(`Wood density (g/cm^3), oven dry mass/fresh volume`))

lu_conv <- read_csv("data/activity data/land_use_names.csv")
lu_conv

sf_country <- st_read("data/GIS/TimorLeste.geoJSON") 

## Load E raster file Download E.nc from: 
## http://chave.ups-tlse.fr/pantropical_allometry/E.nc.zip
rs_envir_stress <- terra::rast("data/GIS/E.nc")

## Load plot AD info
ad_info <- read_csv("data/NFItest/AD_info.csv")

##
## Checks ###################################################################
##

# print(tree_init)
# table(tree_init$plot_no)
# 
# print(sapling_init)
# table(sapling_init$plot_no)
# 
# print(plot_init)
# 
# print(lf_init)
# print(lf_object_init)



##
## Update plot level data ###################################################
##



## Assign land cover based on AD categories
plot <- plot_init %>%
  mutate(
    plot_center_GPS_S = -as.numeric(plot_center_GPS_S),
    plot_center_GPS_E = as.numeric(plot_center_GPS_E),
  ) %>%
  left_join(ad_info, by = "plot_no") %>%
  filter(!is.na(plot_center_GPS_S))

plot$lf_land_cover
plot$corr_land_cover
plot$corr_climate


sf_plot <- plot %>%
  select(plot_no, corr_land_cover, ad_land_cover, nfi_land_cover, plot_center_GPS_E, plot_center_GPS_S) %>%
  st_as_sf(coords = c("plot_center_GPS_E", "plot_center_GPS_S"), crs = 4326)

#st_write(sf_plot, "results/NFItest.geoJSON")

ggplot() +
  geom_sf(data = sf_plot) +
  geom_sf(data = sf_country, fill = NA)


## Crop E to Lao boundaries
sf_plot$envir_stress <- terra::extract(rs_envir_stress, vect(sf_plot))$layer

## Check
sf_plot

##
## Update tree level data ###################################################
##

## Get species scientific names
# nfi_species <- tibble(species = unique(tree_init$tree_species_scientific)) %>%
#   mutate(species = if_else(species == "NA", NA_character_, species)) %>%
#   left_join(gwd_species, by = c("species" = "Binomial"))

tree <- tree_init %>%
  left_join(as_tibble(sf_plot), by = "plot_no") %>%
  left_join(gwd_species, by = c("tree_species_scientific" = "Binomial")) %>%
  mutate(
    tree_wd = if_else(!is.na(wd_mean), wd_mean, 0.57),
    tree_total_height = as.numeric(tree_total_height),
    tree_height_model = exp(0.893 - envir_stress + 0.760 * log(tree_dbh) - 0.0340 * (log(tree_dbh))^2),
    tree_height_ciplus = tree_height_model * (1 + 2 * 0.243),  ## model SD  = 0.243
    tree_height_ciminus = tree_height_model * (1 - 2 * 0.243),  ## model SD  = 0.243
    tree_height_corr = case_when(
      is.na(tree_total_height) ~ tree_height_model,
      tree_total_height < tree_height_ciplus & tree_total_height > tree_height_ciminus ~ tree_total_height,
      TRUE ~ tree_height_model
      ),
    tree_ba = round((tree_dbh/200)^2 * pi, 2),
    tree_agb_chave = 0.0673 * (tree_wd * tree_dbh^2 * tree_height_corr)^0.976, #exp(-1.803 - 0.976 * envir_stress + 0.976 * log(tree_wd) + 2.673 * log(tree_dbh) - 0.0299 * (log(tree_dbh))^2),
    subplot_size = case_when(
      tree_dbh < 10 ~ 3^2 * pi, 
      tree_dbh < 30 ~ 12^2 * pi,
      TRUE          ~ 25^2 * pi
      ),
    scale_factor = 10000 / subplot_size
  ) %>%
  filter(!is.na(tree_dbh))


## Checks
# table(tree$plot_no)
# 
# summary(tree$tree_dbh)
# summary(tree$tree_total_height)
# summary(tree$tree_height_model)
# summary(tree$tree_wd)
# summary(tree$tree_agb_chave)
# summary(tree$envir_stress)


ggplot(tree) +
  geom_point(aes(x = tree_dbh, y = tree_total_height, color = plot_no), shape = 3, size = 2) +
  geom_line(aes(x = tree_dbh, y = tree_height_ciplus, color = plot_no)) +
  geom_line(aes(x = tree_dbh, y = tree_height_ciminus, color = plot_no)) +
  geom_point(aes(x = tree_dbh, y = tree_height_corr, color = plot_no), size = 2) +
  labs(x = "Tree DBH (cm)", y = "Tree height (m)", color = "Plot ID")


ggplot(tree) +
  geom_point(aes(x = tree_dbh, y = tree_agb_chave, color = plot_no))


## 
## Calculate plot and forest type AGB #######################################
##

subplot_agb <- tree %>%
  group_by(plot_no, subplot_no) %>%
  summarise(
    subplot_n_trees    = n(), 
    subplot_n_trees_ha = sum(scale_factor),
    subplot_ba_ha      = sum(tree_ba * scale_factor),
    subplot_agb_ha     = sum(tree_agb_chave * scale_factor * 0.001), ## <- 0.001 to convert kg to ton
    .groups = "drop"
  )

subplot_agb

plot_agb <- subplot_agb %>%
  group_by(plot_no) %>%
  summarise(
    plot_n_trees    = sum(subplot_n_trees), 
    plot_n_trees_ha = mean(subplot_n_trees_ha),
    plot_ba_ha      = mean(subplot_ba_ha),
    plot_agb_ha     = mean(subplot_agb_ha),
    .groups = "drop"
  ) %>%
  left_join(plot, by = "plot_no") %>%
  mutate(
    plot_bgb_ha = case_when(
      corr_climate == "Moist" & plot_agb_ha >= 125 ~ 0.323 * plot_agb_ha, ## IPCC 2019: https://www.ipcc-nggip.iges.or.jp/public/2019rf/pdf/4_Volume4/19R_V4_Ch04_Forest%20Land.pdf
      corr_climate == "Moist" & plot_agb_ha <  125 ~ 0.246 * plot_agb_ha, 
      corr_climate == "Dry" & plot_agb_ha >= 125   ~ 0.440 * plot_agb_ha,
      corr_climate == "Dry" & plot_agb_ha <  125   ~ 0.379 * plot_agb_ha,
    ),
    plot_carbon_ha = (plot_agb_ha + plot_bgb_ha) * 0.47
  )

plot_agb


ftype_agb <- plot_agb %>%
  group_by(corr_land_cover) %>%
  summarise(
    n_plot     = n(),
    agb_all    = round(mean(plot_agb_ha), 3),
    bgb_all    = round(mean(plot_bgb_ha), 3),
    carbon_tot = round(mean(plot_carbon_ha), 3),
    sd_carbon  = sd(plot_carbon_ha),
    .groups = "drop"
  ) %>%
  mutate(
    ci      = sd_carbon / sqrt(n_plot) * round(qt(0.975, n_plot-1), 2),
    ci_perc = round(ci / carbon_tot * 100, 0)
  )
ftype_agb

climate_agb <- plot_agb %>%
  group_by(corr_climate) %>%
  summarise(
    n_plot     = n(),
    agb_all    = round(mean(plot_agb_ha), 3),
    bgb_all    = round(mean(plot_bgb_ha), 3),
    carbon_tot = round(mean(plot_carbon_ha), 3),
    sd_carbon  = sd(plot_carbon_ha),
    .groups = "drop"
  ) %>%
  mutate(
    ci      = sd_carbon / sqrt(n_plot) * round(qt(0.975, n_plot-1), 2),
    ci_perc = round(ci / carbon_tot * 100, 0)
  )
climate_agb

default_agb <- plot_agb %>%
  summarise(
    n_plot     = n(),
    agb_all    = round(mean(plot_agb_ha), 3),
    bgb_all    = round(mean(plot_bgb_ha), 3),
    carbon_tot = round(mean(plot_carbon_ha), 3),
    sd_carbon  = sd(plot_carbon_ha)
  ) %>%
  mutate(
    ci      = sd_carbon / sqrt(n_plot) * round(qt(0.975, n_plot-1), 2),
    ci_perc = round(ci / carbon_tot * 100, 0)
  )
default_agb




