
## +++ +++
## NFI design optimization scripts for Timor Leste
## Gael Sola, FAO
## June 2022
## contact: gaelsola@hotmail.fr
## +++ +++



## NFI Grid ----

offset <- st_bbox(sf_country)[c("xmin", "ymin")] + c(1000, 1000)

sf_grid <- st_make_grid(sf_country, cellsize = c(10000, 10000), what = "polygons", offset = offset) %>%
  st_intersection(sf_country)

sf_points <- st_make_grid(sf_country, cellsize = c(10000, 10000), what = "centers", offset = offset) %>%
  st_intersection(sf_country)

sf_random <- st_sample(x = sf_country, size = nrow(as_tibble(sf_points)), type = 'random')

