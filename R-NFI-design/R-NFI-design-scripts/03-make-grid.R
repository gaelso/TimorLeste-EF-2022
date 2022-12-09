
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

## Calculating pixel area from Biomass map
pix_area <- terra::res(rs_agb)[1]^2 / 100^2


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

grid1 <- make_grid(
  spacing_km = 1, 
  offset = offset, 
  square = F, 
  raster = rs_jica,
  forest_classes = c("Dense forest", "Sparse forest", "Very sparse forest"),
  forest_colors = jica_lc %>% filter(new_code %in% 0:2) %>% pull(hex)
)

grid1$points
grid1$plot



##
##### Making 1 km  grid for Activity Data ###################################
##

# st_coordinates(grid1$plot)[,1]

tt <- grid1$plot %>% 
  mutate(
    x_coord = st_coordinates(.)[,1],
    y_coord = st_coordinates(.)[,2],
    ) %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  select(ID, x_coord, y_coord, lc_jica2010 = lc)

tt2 <- tt %>%
  group_by(lc_jica2010) %>%
  summarise(count = n())

res_path <- paste0("results/grid_square_1km_random_offset_", str_replace(st_crs(grid1$plot)$srid, ":", "_"))
write_csv(tt, paste0(res_path, ".csv"))
write_csv(tt2, paste0(res_path, "_stats_jica2010.csv"))

ggplot() +
  geom_sf(data = sf_country, col = "red", fill = NA) +
  geom_point(data = tt, aes(x = x_coord, y = y_coord, color = lc_jica2010), size = 0.5)
# 
# 
# ggplot() +
#   geom_sf(data = sf_country, col = "red", fill = NA) +
#   geom_sf(data = grid2$plot, aes(color = lc), size = 0.5)
# 
# 
# st_crs(grid1$plot)$srid




# ## Checking mongolia
# tt <- read_csv("data/GIS/test-mongolia/equalDistanceGrid1000meters.csv")
# 
# sf_tt <- st_as_sf(tt, coords = c("X", "Y"), crs = 4326)
# 
# sf_mng         <- st_read("data/GIS/gadm/gadm36_0.shp") %>% filter(GID_0 == "MNG")
# sf_mng_32645   <- st_transform(sf_mng, crs = 32645) #%>% select(geometry)
# sf_points_mng  <- st_make_grid(x = sf_mng_32645, cellsize = c(1000, 1000), what = "centers", square = TRUE)
# 
# 
# sf_points_mng2 <- st_join(sf_points_mng, sf_mng_32645)
# 
# #sf_points_mng3 <- st_intersection(sf_points_mng, sf_mng_32645) 
# #sf_points_mng[st_intersects(sf_points_mng, sf_mng_32645),]
# 
# sf_points_mng2 <- sf_points_mng[st_within(sf_points_mng, sf_mng_32645) %>% lengths > 0, ] 
# 
# #df <- df[st_within(df, box) %>% lengths > 0,]
# 
# ggplot() +
#   geom_sf(data = sf_mng, fill = NA, color = "red") +
#   geom_sf(data = sf_tt, size = 0.5, pch = 1) +
#   geom_sf(data = sf_points_mng, size = 0.5, pch = 3)


#############################################################################


grid2 <- make_grid(
  spacing_km = 2, 
  offset = offset, 
  square = F, 
  raster = rs_jica,
  forest_classes = c("Dense forest", "Sparse forest", "Very sparse forest"),
  forest_colors = jica_lc %>% filter(new_code %in% 0:2) %>% pull(hex)
)

# grid2$n_plot_forest
# sum(grid2$n_plot$n)
# 
# grid2$gr_forest



grid4 <- make_grid(
  spacing_km = 4, 
  offset = offset, 
  square = F, 
  raster = rs_jica,
  forest_classes = c("Dense forest", "Sparse forest", "Very sparse forest"),
  forest_colors = jica_lc %>% filter(new_code %in% 0:2) %>% pull(hex)
  )
# grid4$gr_forest
# grid4$n_plot_forest


grid6 <- make_grid(
  spacing_km = 6, 
  offset = offset, 
  square = F, 
  raster = rs_jica,
  forest_classes = c("Dense forest", "Sparse forest", "Very sparse forest"),
  forest_colors = jica_lc %>% filter(new_code %in% 0:2) %>% pull(hex)
  )
# grid6$gr_forest
# grid6$n_plot_forest

grid8 <- make_grid(
  spacing_km = 8, 
  offset = offset, 
  square = F, 
  raster = rs_jica,
  forest_classes = c("Dense forest", "Sparse forest", "Very sparse forest"),
  forest_colors = jica_lc %>% filter(new_code %in% 0:2) %>% pull(hex)
  )
# grid8$gr_forest +
#   geom_sf(data = grid8$points[sf_country, ], size = 0.6)
# grid8$n_plot_forest
# 
# n_plot05
# n_plot10


## Overlap grids

# ggplot() +
#   geom_sf(data = sf_country, fill = NA, size = 1) +
#   geom_sf(data = grid8$grid, fill = NA, color = "darkorange", size = 0.6) +
#   geom_sf(data = grid4$grid, fill = NA, color = "orange") +
#   labs(fill = NULL)
  

## END ##
