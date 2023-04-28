
library(sf)
library(tmap)
library(tidyverse)
library(dggridR)

theme_set(theme_bw())

list.files("results")

sf_aoi <- st_read("data/spatial/TimorLeste.geoJSON") 
sf_aoi <- sf_aoi %>% 
  st_transform(4326) %>%
  mutate(name = "Timor Leste")
sf_sbae <- st_read("data/spatial/01_sbae_points.gpkg")

ce <- read_csv("results/sbae_2km_TL_clean_2023-03-23.csv")

lu_conv <- tibble(
  lu = c("Cropland", "Forest", "Grassland", "Otherland", "Settlement", "Wetland"),
  lu_no = c(3, 1, 2, 6, 4, 5),
  lu_f = fct_reorder(lu, lu_no)
)

sort(unique(ce$land_use_subdivision_label))

lu_cat <- tibble(
  lu_cat = c(
    "Moist high land forest", "Moist lowland forest", "Dry lowland forest", 
    "Montane forest", "Coastal forest", "Mangroves", "Forest plantation", 
    "grassland", "Shrubs", "Other wooded land",
    "Cropland",
    "Settlement", "Infrastructure", 
    "Wetlands", "Lakes/Lagoons/Reservoirs", "River", 
    "Mining", "Rocks", "Sand", "Other bareland"  
    ),
  lu_cat_no = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 30, 41, 42, 51, 52, 53, 61, 62, 63, 64),
  lu_cat_f = fct_reorder(lu_cat, lu_cat_no)
)

lu_frl <- tibble(
  lu_frl = c(
    "Moist high land forest", "Moist lowland forest", "Dry lowland forest", 
    "Montane forest", "Coastal forest", "Mangroves", "Forest plantation", 
    "grassland", "Shrubs", "Other wooded land",
    "Cropland",
    "Settlements", 
    "Wetlands", 
    "Bareland"  
  ),
  lu_frl_no = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 30, 40, 50, 60),
  lu_frl_f = fct_reorder(lu_frl, lu_frl_no)
)

table(lu_cat$lu_cat_f)

## Color codes
color_codes <- tribble(
  ~color   , ~lu_cat                                          , ~comment,
  '#0a2dd5',	"Moist lowland forest - very dense"             , "mean height 250 m - wettest part of the valleys and very dense low areas (e.g Los palos) (Oki, Calisto)",
  '#db0014',	"Dry lowland forest - sparse"                   , "mean heigh 300 m - low parts of the country, as river valleys  (Albino)",
  '#ff8f1c',	"Dry lowland forest - mix classes"              , "mean height 250 m - some croplands and settlements (Virgilio)",
  '#f1ff18',  "Mostly croplands, shifting cultivations"       , "mean height 350m (Calisto)",
  '#ff380a',	"Dry lowland forest - sparse"                   , "mean height 350 m - lowland areas of the valleys in the western part of the country (Celeste)",
  '#7a8bff',	"Moist lowland forest -  dense"                 , "mean height 400 m (Marcia)",
  '#04a00d',	"Moist  highland forest - very dense"           , "mean height 700 m (Elvino)",
  '#1fff10',	"Moist highland forest - sparse"                , "some plots are shrubs - mean height 800 m (Oki) // DIVIDE IN TWO CLASSES",
  '#04a00d',  "Moist highland forest - very dense"            , "mean height 900m - (Virgilio and Albino) - JOIN WITH 6",
  '#28b9ff',  "Moist lowland forest  medium"                  , "mean height 300 m - on the wet valleys of the river(Calisto)",
  '#ff4be9',  "Dry lowland forest - very sparse"              , "mean height 300m (Celeste)",
  '#4d63ff',  "Moist lowland forest - dense"                  , "just a bit less than class 0 - mean height 600 m (Albino) - JOIN WITH CLASS 0",
  '#bdf0ff',  "Moist lowland forest sparse"                   , "mean heigh 300m - coastal areas in the southern part of the country (Celeste)",
  '#0f6f09',  "Moist mountain or highland forest - very dense", "mean height 1300 m (Elvino)",
  '#aa6510',  "Shrub"                                         , "mean heigh 400m (Albino)"
)


ce_points <- sf_sbae %>%
  as_tibble() %>%
  rename(id = PLOTID, seqnum = name) %>%
  mutate(seqnum = as.numeric(seqnum)) %>%
  left_join(ce, ., by = "id") %>%
  left_join(lu_conv, by = c("land_use_category_label" = "lu")) %>%
  left_join(lu_cat, by = c("land_use_subdivision_label" = "lu_cat"))


table(ce_points$land_use_category_label)
table(ce_points$lu_f)
table(ce_points$land_use_subdivision_label)
table(ce_points$lu_cat_f, useNA = "ifany")
table(ce_points$lu_f, ce_points$lu_cat_f, useNA = "ifany")

## Make grid for AD
res      <- 15
bbox_aoi <- st_bbox(sf_aoi)
dggs     <- dgconstruct(res = res)
dist     <- dggridR:::GridStat_cellDistKM(dggs[['projection']], dggs[['topology']], dggs[['aperture']], res)
dist_deg <- round(dist / 111 * 0.75, 4) ## calc degree with some margin of error

sf_grid_init <- dgrectgrid(
  dggs, 
  minlat = floor(bbox_aoi$ymin), 
  minlon = floor(bbox_aoi$xmin), 
  maxlat = ceiling(bbox_aoi$ymax), 
  maxlon = ceiling(bbox_aoi$xmax), 
  cellsize = dist_deg
  )  ## bounding box for Timor Leste

vec_grid_num <- st_intersection(sf_grid_init, sf_aoi) %>% pull(seqnum)

sf_grid_large <- sf_grid_init %>% filter(seqnum %in% vec_grid_num)

gr_grid_large <- ggplot() +
  geom_sf(data = sf_aoi, fill = "grey80", color = NA) +
  geom_sf(data = sf_grid_large, fill = NA, color = "darkred")

## Reduce grid
sf_point <- st_centroid(sf_grid_large) %>% 
  st_join(sf_aoi) %>% 
  filter(!is.na(name)) %>%
  mutate(
    x = st_coordinates(.)[,1], 
    y = st_coordinates(.)[,2]
  )

sf_grid <- sf_grid_large %>% filter(seqnum %in% sf_point$seqnum)

st_write(sf_grid, "data/spatial/dggrid15_TL.gesoJSON")

## Check grid
ggplot() +
  geom_sf(data = sf_aoi, fill = "grey80", color = NA) +
  geom_sf(data = sf_grid, fill = NA, color = "darkred")

## ADD CE info
sf_grid_ce <- sf_grid %>%
  left_join(ce_id, by = "seqnum") %>%
  left_join(lu_conv, by = c("land_use_category_label" = "lu"))



table(lu_conv$lu_f)

table(sf_grid_ce$land_use_category_label, useNA = "ifany")

ggplot() +
  geom_sf(data = sf_aoi, fill = "grey80", color = NA) +
  geom_sf(data = sf_grid_ce, aes(fill = land_use_category_label), color = NA)
  

sort(sf_sbae$name)
sort(sf_grid$seqnum)
# tmap_mode("view")
# tm_basemap(c("Esri.WorldGrayCanvas", "Esri.WorldTopoMap", "Esri.WorldImagery")) +
#   tm_shape(sf_aoi) + tm_borders()

