
## NFI Design
## Gael Sola, FAO, May 2022


## Libraries

library(sf)
library(stars)
library(terra)
library(tidyverse)
library(tmap)
library(ggspatial)

ggplot2::theme_set(theme_bw())

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


## + NFI Grid ----

offset <- st_bbox(sf_country)[c("xmin", "ymin")] + c(1000, 1000)

sf_grid <- st_make_grid(sf_country, cellsize = c(10000, 10000), what = "polygons", offset = offset) %>%
  st_intersection(sf_country)

sf_points <- st_make_grid(sf_country, cellsize = c(10000, 10000), what = "centers", offset = offset) %>%
  st_intersection(sf_country)

sf_random <- st_sample(x = sf_country, size = nrow(as_tibble(sf_points)), type = 'random')


## + JICA land cover ----

rs_jica_init <- rast("data/GIS/LUFC2012_Raster30_FEB20131/LUFC2012_Raster30_FEB20131.tif")
summary(rs_jica_init)
plot(rs_jica_init)
levels(rs_jica_init)
cats(rs_jica_init)

jica_code <- tibble(
  old_code = c(1, 3,  4,  5,  7,  9, 21, 22, 23, 27),
  new_code = c(0, 1, 9, 3, 4, 5, 6, 8, 7, 2)
)

jica_lc <- tibble(
  lc = c("Dense forest", "Sparse forest", "Unknown", "Grassland", "Bareland", 
         "Inland Water", "Rice field", "Dry field", "Settlement", "Very sparse forest"),
  hex = c("#009900", "#00ff00", "#ff0000", "#ffccff", "#cc00ff", 
          "#0033ff", "#cccc00", "#ff9933", "#330000", "#99ff99")
)

jica_lc <- tibble(
  new_code = c(0, 1, 9, 3, 4, 5, 6, 8, 7, 2),
  lc = c("Dense forest", "Sparse forest", "Unknown", "Grassland", "Bareland", 
         "Inland Water", "Rice field", "Dry field", "Settlement", "Very sparse forest"),
  hex = c("#009900", "#00ff00", "#ff0000", "#ffccff", "#cc00ff", 
          "#0033ff", "#cccc00", "#ff9933", "#330000", "#99ff99")
) %>%
  arrange(new_code)

rs_jica <- classify(rs_jica_init, jica_code)
levels(rs_jica) <- jica_lc$lc
names(rs_jica) <- "lc"
plot(rs_jica, col=jica_lc$hex)
summary(rs_jica)
cats(rs_jica)
rs_jica

rs_jica2 <- st_as_stars(rs_jica)

rs_jica3 <- as.data.frame(rs_jica, xy = TRUE) %>% na.omit()


## Vizu ----


## + Leaflet ----

tmap_mode("view")

#tm_basemap("OpenTopoMap") +
#tm_basemap("GeoportailFrance.orthos") +
tm_basemap("Esri.WorldImagery") +
tm_shape(sf_plot) + tm_dots(col = "forest_type", title = "Forest type", size = 0.1) +
#tm_shape(sf_points) + tm_dots(size = 0.02, col = "darkred") +
tm_shape(sf_random) + tm_dots(size = 0.02, col = "purple") +
tm_shape(sf_grid) + tm_borders(col = "red") +
tm_shape(sf_country) + tm_borders(col = "red") +
tm_shape(rs_jica) + tm_raster(style = "cont", palette = jica_lc$hex, legend.show = TRUE)


## + ggplot ----

ggplot() +
  geom_sf(data = sf_points) +
  geom_sf(data = sf_grid, fill = NA, col = "red", size = 0.1) +
  geom_sf(data = sf_country, fill = NA, col = "red", size = 1) +
  geom_raster(data = rs_jica) +
  scale_fill_manual(values = jica_lc$hex)

ggplot() +
  geom_raster(data = rs_jica3, aes(x = x, y = y, fill = lc)) +
  scale_fill_manual(values = jica_lc$hex)

# ggplot() +
#   geom_stars(data = rs_jica2)
