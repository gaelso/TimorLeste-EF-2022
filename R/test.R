
library(dggridR)
library(sf)
library(tidyverse)

theme_set(theme_bw())

sf_aoi <- st_read("data/GIS/TimorLeste.shp")

dggs <- dgconstruct(res = 15, resround = "down")

dggs$res

dist <- dggridR:::GridStat_cellDistKM(dggs[['projection']], dggs[['topology']], dggs[['aperture']], dggs$res)
dist

dist_deg <- round(dist / 111 * 0.75, 4) ## calc degree with some margin of error
dist_deg

sf_grid <- dgshptogrid(dggs, "data/GIS/TimorLeste.shp", savegrid = NA, cellsize = dist_deg)

ggplot() +
  geom_sf(data = sf_aoi, fill = NA, ) +
  geom_sf(data = sf_grid, fill = alpha("darkred", alpha = 0.4), color = NA)
