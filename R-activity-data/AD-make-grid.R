
library(tidyverse)

# remotes::install_github("r-barnes/dggridR", vignette=TRUE)
library(dggridR)

ggplot2::theme_set(theme_bw())  

## Get resolution equivalent table / idk why need to set a dgconstruct().
dggrid_table <- as_tibble(dggridR::dggetres(dggridR::dgconstruct(res = 0)))
dggrid_table



##
## Manually recreate the grid from function code ###############################
## https://github.com/r-barnes/dggridR/blob/master/R/dggridR.R
##

## Load AOI
sf_aoi <- st_read("data/GIS/admin/TimorLeste.shp") 
sf_aoi_metric <- st_transform(sf_aoi, "ESRI:54017")

## Create a grid setup
dg_init <- dggridR::dgconstruct(res = 15, metric = TRUE)

## Create points matching the grid resolution
dg_spacing <- dggrid_table %>%
  filter(res == dg_init$res) %>%
  mutate(spacing_m = spacing_km * 1000) %>%
  pull(spacing_m)

dg_approx_points <- sf::st_make_grid(sf_aoi_metric, cellsize = c(dg_spacing, dg_spacing), square = F, what = "centers") %>%
  sf::st_intersection(sf_aoi_metric) %>%
  st_transform(4326)

ggplot() + 
  geom_sf(data = dg_approx_points, size = 0.2) +
  geom_sf(data = sf_aoi_metric, fill = NA, col= "red") +
  coord_sf(crs = "ESRI:54017")

## Convert the points to SEQNUM ids for identifying which grid cells to keep
dg_seqnum <- dgGEO_to_SEQNUM(dg_init, st_coordinates(dg_approx_points)[,1], st_coordinates(dg_approx_points)[,2])

## get matching grid cells
dg_grid <- dgcellstogrid(dg_init, dg_seqnum$seqnum)
dg_grid_metric <- st_transform(dg_grid, "ESRI:54017")

## Check
ggplot() + 
  geom_sf(data = dg_grid_metric, fill = NA) +
  geom_sf(data = sf_aoi_metric, fill = NA, col= "red")



##
## Create hex grid cells for the Collect Earth results #########################
##

ce_points <- read_csv("results/AD_points_fromCE.csv")

sf_cepoints <- st_as_sf(ce_points, coords = c("location_x", "location_y"), crs = unique(ce_points$location_srs))

dg_seqnum <- dgGEO_to_SEQNUM(dg_init, st_coordinates(sf_cepoints)[,1], st_coordinates(sf_cepoints)[,2])

dg_grid <- dgcellstogrid(dg_init, dg_seqnum$seqnum)
dg_grid_metric <- st_transform(dg_grid, "ESRI:54017")
