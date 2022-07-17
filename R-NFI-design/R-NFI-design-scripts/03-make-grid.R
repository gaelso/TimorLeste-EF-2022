
## +++ +++
## NFI design optimization scripts for Timor Leste
## +++ +++


## Sampling Characteristics ----

## Sampling design options: systematic sampling
## Multi-stages: (TBD, initially simple stage)
## Annual vs periodic: Periodic
## Grid shape: Hexagonal



## Sampling size ----

## + Calc AGB CV based on Avitabile 2016 map ----

## Combine land use classes and agb raster data
agb_group <- df_jica100 %>%
  left_join(df_agb) %>%
  mutate(agb = if_else(is.na(agb_avitabile), 0, agb_avitabile))
agb_group

## Calc stats
agb_tot <- agb_group %>%
  summarise(
    n_pix = n(),
    agb_mean = round(mean(agb), 2),
    agb_sd = round(sd(agb), 2),
  ) %>%
  mutate(
    ci      = round(agb_sd / sqrt(n_pix) * 1.96, 2),
    ci_perc = round(ci / agb_mean * 100, 0),
    lc = "Total"
  ) %>%
  select(lc, everything())

agb_class <- agb_group %>%
  group_by(lc) %>%
  summarise(
    n_pix = n(),
    agb_mean = round(mean(agb), 2),
    agb_sd = round(sd(agb), 2),
  ) %>%
  mutate(
    ci      = round(agb_sd / sqrt(n_pix) * 1.96, 2),
    ci_perc = round(ci / agb_mean * 100, 0)
  ) %>%
  bind_rows(agb_tot)
agb_class



## + Calc sampling size ----

## Random and systematic designs
n_plot05 <- round(((agb_tot$agb_sd / agb_tot$agb_mean * 100) * 1.96 / 5)^2)
n_plot05

n_plot10 <- round(((agb_tot$agb_sd / agb_tot$agb_mean * 100) * 1.96 / 10)^2)
n_plot10  

## Calc area of forest land based on pixel count (100x100)
area_forest <- agb_class %>%
  filter(lc %in% c("Dense forest", "Sparse forest", "Very sparse forest")) %>%
  pull(n_pix) %>%
  sum()
area_forest  
  
## Calc area of forest land based on pixel count (30x30)
area_forest <- df_jica %>%
  filter(lc %in% c("Dense forest", "Sparse forest", "Very sparse forest")) %>%
  summarise(n_pix = n()) %>%
  mutate(area = n_pix * 30^2 / 100^2)
area_forest  

area_forest_km2 <- as.numeric(set_units(area_forest, value = km2))

grid_spacing <- round(sqrt(area_forest_km2 / n10), 3)


## NFI Grid ----

offset <- st_bbox(sf_country)[c("xmin", "ymin")] + c(1000, 1000)

sf_grid <- st_make_grid(sf_country, cellsize = c(10000, 10000), what = "polygons", offset = offset) %>%
  st_intersection(sf_country)

sf_points <- st_make_grid(sf_country, cellsize = c(10000, 10000), what = "centers", offset = offset) %>%
  st_intersection(sf_country)

sf_random <- st_sample(x = sf_country, size = nrow(as_tibble(sf_points)), type = 'random')

