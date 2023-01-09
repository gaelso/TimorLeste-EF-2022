
library(tidyverse)

# remotes::install_github("r-barnes/dggridR", vignette=TRUE)
library(dggridR)

ggplot2::theme_set(theme_bw())  

## Get resolution equivalent table / idk why need to set a dgconstruct().
# dggrid_table <- as_tibble(dggridR::dggetres(dggridR::dgconstruct(res = 0)))
# dggrid_table
# 
# write_csv(dggrid_table, "data/GIS/dggrid_resolutions.csv")
dggrid_table <- read_csv("data/GIS/dggrid_resolutions.csv")
dggrid_table
dggrid_table %>% filter(res == 15) 
dggrid_table %>% filter(res == 15) %>% pull(area_km) 

## Load AOI
sf_aoi <- st_read("data/GIS/admin/TimorLeste.shp") 
sf_aoi_metric <- st_transform(sf_aoi, "ESRI:54017")

## Create a grid setup
dg_init <- dggridR::dgconstruct(res = 15, metric = TRUE)

##
## Initiate grid and load data #################################################
##

##
## Manually recreate the grid from function code ###############################
## https://github.com/r-barnes/dggridR/blob/master/R/dggridR.R
##

## Create points matching the grid resolution
# dg_spacing <- dggrid_table %>%
#   filter(res == dg_init$res) %>%
#   mutate(spacing_m = spacing_km * 1000) %>%
#   pull(spacing_m)
# 
# dg_approx_points <- sf::st_make_grid(sf_aoi_metric, cellsize = c(dg_spacing, dg_spacing), square = F, what = "centers") %>%
#   sf::st_intersection(sf_aoi_metric) %>%
#   st_transform(4326)
# 
# ggplot() + 
#   geom_sf(data = dg_approx_points, size = 0.2) +
#   geom_sf(data = sf_aoi_metric, fill = NA, col= "red") +
#   coord_sf(crs = "ESRI:54017")
# 
# ## Convert the points to SEQNUM ids for identifying which grid cells to keep
# dg_seqnum <- dgGEO_to_SEQNUM(dg_init, st_coordinates(dg_approx_points)[,1], st_coordinates(dg_approx_points)[,2])
# 
# ## get matching grid cells
# dg_grid <- dgcellstogrid(dg_init, dg_seqnum$seqnum)
# dg_grid_metric <- st_transform(dg_grid, "ESRI:54017")
# 
# ## Check
# ggplot() + 
#   geom_sf(data = dg_grid_metric, fill = NA) +
#   geom_sf(data = sf_aoi_metric, fill = NA, col= "red")
# 


##
## Create hex grid cells for the Collect Earth results #########################
##

ce_points <- read_csv("results/CE_points_clean.csv")

sf_cepoints <- st_as_sf(ce_points, coords = c("location_x", "location_y"), crs = unique(ce_points$location_srs)) %>%
  st_transform(4326)

table(sf_cepoints$redd_activity)

dg_seqnum <- dgGEO_to_SEQNUM(dg_init, st_coordinates(sf_cepoints)[,1], st_coordinates(sf_cepoints)[,2])

sf_cepoints <- sf_cepoints %>% mutate(seqnum = dg_seqnum$seqnum)

sf_cepoints$seqnum

sf_dg_grid <- dgcellstogrid(dg_init, dg_seqnum$seqnum) %>%
  left_join(as_tibble(sf_cepoints), by = "seqnum")

# sf_dg_grid <- dgcellstogrid(dg_init, dg_seqnum$seqnum) %>%
#   st_join(sf_cepoints, join = st_contains)

sf_dg_grid_metric <- st_transform(sf_dg_grid, "ESRI:54017")

## Check
ggplot() + 
  geom_sf(data = sf_dg_grid_metric, aes(fill = redd_activity)) +
  #geom_sf(data = sf_cepoints, aes(fill = redd_activity), size = 1, pch = 21) +
  geom_sf(data = sf_aoi_metric, fill = NA, col= "red") +
  scale_fill_manual(values = c("green", "red", "grey80", "grey20")) +
  labs(subtitle = "All changes 2000-2022")

##
## Create meaningfull maps #####################################################
##

## Limit data to the FRL period
table(sf_cepoints$lu_change_year, useNA = "ifany")

sf_ad <- sf_dg_grid_metric %>%
  mutate(
    lu_change_year = if_else(lu_change_year>= 2017, lu_change_year, NA_real_),
    redd_activity  = if_else(is.na(lu_change_year), "No change", redd_activity)
  )

sf_df <- sf_ad %>% 
  filter(redd_activity == "DF")
  
sf_af <- sf_ad %>% 
  filter(redd_activity == "AF")

## Check data
table(sf_ad$lu_change_year, useNA = "ifany")
table(sf_ad$redd_activity, useNA = "ifany")
table(sf_dg_grid_metric$redd_activity, useNA = "ifany")

## Make graph
ggplot() +
  geom_sf(data = sf_df, aes(fill = lu_change_year)) +
  geom_sf(data = sf_aoi_metric, fill = NA, col= "red") +
  scale_fill_viridis_c() +
  coord_sf(crs = 32751) + #coord_sf(crs = "ESRI:54017")
  labs(fill = "Year of deforestation")
  
ggplot() +
  geom_sf(data = sf_af, aes(fill = lu_change_year)) +
  geom_sf(data = sf_aoi_metric, fill = NA, col= "red") +
  scale_fill_viridis_c() +
  coord_sf(crs = 32751) + #coord_sf(crs = "ESRI:54017")
  labs(fill = "Year of afforestation")

ggplot() +
  geom_sf(data = sf_af, aes(fill = lu_change_year)) +
  geom_sf(data = sf_aoi_metric, fill = NA, col= "red") +
  scale_fill_viridis_c() +
  coord_sf(crs = 32751) + #coord_sf(crs = "ESRI:54017")
  labs(fill = "Year of afforestation")


ggplot() + 
  geom_sf(data = sf_ad, aes(fill = redd_activity)) +
  geom_sf(data = sf_aoi_metric, fill = NA, col= "red") +
  scale_fill_manual(values = c("green", "red", "grey80", "grey20")) +
  labs(subtitle = "FRL changes 2017-2022")




