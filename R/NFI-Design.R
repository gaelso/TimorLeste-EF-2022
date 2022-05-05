
## NFI Design
## Gael Sola, FAO, May 2022


## Libraries

library(sf)
library(stars)
library(tidyverse)
library(tmap)


## Spatial data ------

## + CF plots ------

plot <- read_csv("data/CF/plot.csv") 

sf_plot <- plot %>%
  mutate(
    forest_type = case_when(
      site == "PGA" ~ "Dense Natural Forest",
      plot_id %in% c("PMT01", "PMT02", "PMT06") ~ "Dense Natural Forest",
      plot_id %in% c("PMT03", "PMT04", "PMT05") ~ "Dense Natural Forest",  
      plot_id %in% c("PSL01", "PSL02", "PSL03") ~ "Dense Natural Forest",
      plot_id %in% c("PSL04", "PSL05")          ~ "Dense Plantation Forest",
    )
  ) %>%
  filter(!is.na(plot_x), !is.na(plot_y)) %>%
  st_as_sf(coords = c("plot_x", "plot_y"))

sf_plot

st_crs(sf_plot) <- 4326


## + Admin ----

sf_district <- st_read("data/GIS/Districts/13districts.shp")

sf_country <- sf_district %>% summarise()


## + NFI Grid

offset <- st_bbox(sf_country)[c("xmin", "ymin")] + c(1000, 1000)

sf_g <- st_make_grid(sf_country, cellsize = c(10000, 10000), what = "polygons", offset = offset) %>%
  st_intersection(sf_country)

## + JICA land cover

rs_jica <- stars::read_stars("data/GIS/LUFC2012_Raster30_FEB20131/LUFC2012_Raster30_FEB20131.tif")

## Vizu ----

tmap_mode("view")

#tm_basemap("OpenTopoMap") +
#tm_basemap("GeoportailFrance.orthos") +
tm_basemap("Esri.WorldImagery") +
tm_shape(sf_plot) + tm_dots(col = "forest_type", title = "Forest type", size = 0.1) +
tm_shape(sf_g) + tm_borders(col = "red") +
tm_shape(sf_country) + tm_borders(col = "red") +
tm_shape(rs_jica) + tm_raster(style = "cont", palette = "YlGn", legend.show = TRUE)


