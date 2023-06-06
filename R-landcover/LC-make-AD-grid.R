
library(dggridR)
library(sf)
library(tidyverse)

theme_set(theme_bw())

sf_aoi <- st_read("data/spatial/TimorLeste.geoJSON")
sf_aoi <- sf_aoi %>% mutate(name = "TimorLeste")
res    <- 15

if (st_crs(sf_aoi)$input != "WGS 84") sf_aoi <- st_transform(sf_aoi, 4326)
  
bbox_aoi <- st_bbox(sf_aoi)
dggs     <- dgconstruct(res = res)
dist     <- dggridR:::GridStat_cellDistKM(dggs[['projection']], dggs[['topology']], dggs[['aperture']], res)
dist_deg <- round(dist / 111 * 0.75, digits = 4) ## calc degree with some margin of error

sf_grid_init <- dgrectgrid(
  dggs     = dggs, 
  minlat   = floor(bbox_aoi$ymin), 
  minlon   = floor(bbox_aoi$xmin), 
  maxlat   = ceiling(bbox_aoi$ymax), 
  maxlon   = ceiling(bbox_aoi$xmax), 
  cellsize = dist_deg
  )
  
vec_grid_num <- st_intersection(sf_grid_init, sf_aoi) %>% pull(seqnum)
  
sf_grid_large <- sf_grid_init %>% filter(seqnum %in% vec_grid_num)
  
gr_grid_large <- ggplot() +
  geom_sf(data = sf_aoi, fill = "grey80", color = NA) +
  geom_sf(data = sf_grid_large, fill = "darkred", color = NA, alpha = 0.3)

print(gr_grid_large)

## Keep only points that are within boundaries
sf_point <- st_centroid(sf_grid_large) %>% 
  st_join(sf_aoi) %>% 
  filter(!is.na(name)) %>%
  mutate(
    x = st_coordinates(.)[,1], 
    y = st_coordinates(.)[,2]
  )

sf_grid <- sf_grid_large %>% filter(seqnum %in% sf_point$seqnum)
  
## Check grid
gr_grid <- ggplot() +
  geom_sf(data = sf_aoi, fill = "grey80", color = NA) +
  geom_sf(data = sf_grid, fill = NA, color = "darkred") +
  geom_sf(data = sf_point, size = 0.6, color = "grey20")

print(gr_grid)

st_write(sf_grid,  "results/grid-ISEA3H-res15.geoJSON")
st_write(sf_point, "results/grid-ISEA3H-res15-points.geoJSON")
