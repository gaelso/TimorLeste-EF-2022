
library(sf)
library(tidyverse)

list.files("data/NFItest")

sf_aoi  <- st_read("data/spatial/TimorLeste.geoJSON")
sf_plot <- st_read("data/NFItest/NFI TESTING_40plots.kml", layer = "NFI testing to measure")

sf_plot_proj <- sf_plot %>% 
  filter(Name %in% c("Z07T02", "Z07T03", "Z07T04", "Z06T01", "Z06T02", "Z06T03", "Z06T04")) %>%
  st_transform("ESRI:54017") %>%
  mutate(
    x_m = st_coordinates(.)[,1],
    y_m = st_coordinates(.)[,2]
  ) %>%
  select(-Description) %>%
  st_zm()


sf_subplot_C <- sf_plot %>%
  mutate(Name = paste0(Name, "C")) %>%
  select(-Description) %>%
  st_zm()

sf_subplot_N1 <- sf_plot_proj %>%
  as_tibble() %>%
  mutate(
    Name = paste0(Name, "N1"),
    y_m = y_m + 50
    ) %>%
  st_as_sf(coords = c("x_m", "y_m"), crs = "ESRI:54017") %>%
  st_transform(4326)

sf_subplot_N2 <- sf_plot_proj %>%
  as_tibble() %>%
  mutate(
    Name = paste0(Name, "N2"),
    y_m = y_m + 100
    ) %>%
  st_as_sf(coords = c("x_m", "y_m"), crs = "ESRI:54017") %>%
  st_transform(4326)

sf_subplot_E1 <- sf_plot_proj %>%
  as_tibble() %>%
  mutate(
    Name = paste0(Name, "E1"),
    x_m = x_m + 50
  ) %>%
  st_as_sf(coords = c("x_m", "y_m"), crs = "ESRI:54017") %>%
  st_transform(4326)

sf_subplot_E2 <- sf_plot_proj %>%
  as_tibble() %>%
  mutate(
    Name = paste0(Name, "E2"),
    x_m = x_m + 100
  ) %>%
  st_as_sf(coords = c("x_m", "y_m"), crs = "ESRI:54017") %>%
  st_transform(4326)

sf_subplot <- bind_rows(sf_subplot_C, sf_subplot_E1, sf_subplot_E2, sf_subplot_N1, sf_subplot_N2) %>%
  st_cast("POINT")

# ggplot() + 
#   geom_sf(data = sf_aoi) +
#   geom_sf(data = sf_subplot) +
#   coord_sf(xlim = c(125, 127), ylim = c(-7, -9))


st_write(sf_subplot, "results/Z06Z07.kml", append = F)
st_write(sf_subplot, "results/Z06Z07.gpx", dataset_options="GPX_USE_EXTENSIONS=yes")
