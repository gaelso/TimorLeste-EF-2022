
library(sf)
library(dggridR)
library(tidyverse)

theme_set(theme_bw())

source("R-functions/make_dggrid.R")

## Check files
list.files("data/spatial")

## Load country boundaries
sf_aoi <- st_read("data/spatial/TimorLeste.geoJSON") %>% 
  st_transform(crs = 4326) %>%
  mutate(name = "TLS")


## QUICK calculation of grid spacing
## Cochran: n = (t*cv/E)^2


cv_agb          <- 80
allowable_error <- 10
forest_prop     <- 0.5
aoi_area        <- sf_aoi %>% mutate(area_m2 = as.numeric(st_area(.))) %>% pull(area_m2)

n_plot <- ceiling(
  (cv_agb * stats::qt(.975, df=Inf) / allowable_error)^2
)

grid_spacing <- sqrt(aoi_area * 10^(-6) / n_plot)

res     <- 13
spacing <- 6.1

TL_grid11 <- make_dggrid(.sf_aoi = sf_aoi, .res = res, .aoi_class = name, .extract_class = T, .msg = F)
TL_grid11

gr <- ggplot() +
  geom_sf(data = sf_aoi, fill = "grey90", color = NA) +
  geom_sf(data = TL_grid11$grid, fill = NA, color = "darkred") +
  geom_sf(data = TL_grid11$point, size = 0.6) +
  labs(
    title    = "Discrete Global Grid over Timor Leste",
    subtitle = paste0("Resolution: ", res, ", avg spacing: ", spacing, "km")
    )

ggsave(plot = gr, filename = "results/TLS_grid13.png", width = 20, height = 15, units = "cm", dpi = 600)

