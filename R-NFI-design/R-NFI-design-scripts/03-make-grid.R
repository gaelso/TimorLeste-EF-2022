
## +++ +++
## NFI design optimization scripts for Timor Leste
## +++ +++


## + Define sampling characteristics ----

## Sampling design options: systematic sampling
## Multi-stages: (TBD, initially simple stage)
## Annual vs periodic: Periodic
## Grid shape: Hexagonal



## + Determine sampling size ----

## + + Calc AGB CV based on Avitabile 2016 map ----

## Combine land use classes and agb raster data
agb_group <- df_jica100 %>%
  left_join(df_agb, by = c("x", "y")) %>%
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



## + + Calc sampling size ----

## Number of plots in random and systematic designs

## 5% desired precision
n_plot05 <- round(((agb_tot$agb_sd / agb_tot$agb_mean * 100) * qt(.975, df=Inf) / 5)^2)
n_plot05

## 10% desired precision
n_plot10 <- round(((agb_tot$agb_sd / agb_tot$agb_mean * 100) * qt(.95, df=Inf) / 10)^2)
n_plot10

## Country area in km^2 (JICA map res = 30x30 m)
area_country <- round(nrow(df_jica) * 30^2 / 1000^2, 0)
area_country

## Calc area of forest in km^2 (JICA map res = 30x30 m)
area_forest <- df_jica %>%
  filter(lc %in% c("Dense forest", "Sparse forest", "Very sparse forest")) %>%
  summarise(n_pix = n()) %>%
  mutate(area = round(n_pix * 30^2 / 1000^2, 0)) %>%
  pull(area)
area_forest  

## Approximate grid spacing to reach n_plot
grid_spacing <- round(sqrt(area_forest / n_plot10), 3)
grid_spacing

grid_spacing <- round(sqrt(area_forest / n_plot05), 3)
grid_spacing



## + Make NFI Grid ----

## Spacing value to be tested and confirmed with number of plots in forest land

## ++ Define random offset ----

set.seed(10)
x_init <- sample(1:1000, 1)
y_init <- sample(1:1000, 1)

offset <- st_bbox(sf_country)[c("xmin", "ymin")] + c(x_init, y_init)



## ++ Test grids 4, 6 and 8 km ---

sf_grid4 <- make_grid(
  spacing_km = 4, 
  offset = offset, 
  square = F, 
  raster = rs_jica,
  forest_classes = c("Dense forest", "Sparse forest", "Very sparse forest"),
  forest_colors = jica_lc %>% filter(new_code %in% 0:2) %>% pull(hex)
  )
sf_grid4$gr_forest
sf_grid4$n_plot_forest


sf_grid6 <- make_grid(
  spacing_km = 6, 
  offset = offset, 
  square = F, 
  raster = rs_jica,
  forest_classes = c("Dense forest", "Sparse forest", "Very sparse forest"),
  forest_colors = jica_lc %>% filter(new_code %in% 0:2) %>% pull(hex)
  )
sf_grid6$gr_forest
sf_grid6$n_plot_forest

sf_grid8 <- make_grid(
  spacing_km = 8, 
  offset = offset, 
  square = F, 
  raster = rs_jica,
  forest_classes = c("Dense forest", "Sparse forest", "Very sparse forest"),
  forest_colors = jica_lc %>% filter(new_code %in% 0:2) %>% pull(hex)
  )
sf_grid8$gr_forest
sf_grid8$n_plot_forest

n_plot05
n_plot10


## END ##
