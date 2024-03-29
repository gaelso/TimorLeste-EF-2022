## Metadata

## Community forestry data analysis
## GCF project: 
## REDD+ Readiness Support to Timor-Leste: Capacity and Institutional Development 
## April 2022




## Libraries ----

library(tidyverse)
library(sf)
library(terra)


## Data ----

plot_init <- read_csv("data/CF/plot.csv", show_col_types = FALSE)
tree_init <- read_csv("data/CF/tree.csv", show_col_types = FALSE)
sapling_init <- read_csv("data/CF/sapling.csv", show_col_types = FALSE)
seedling_init <- read_csv("data/CF/seedling.csv", show_col_types = FALSE)
species_init <- read_csv("data/CF/species-list.csv", show_col_types = FALSE)

## Load E raster file Download E.nc from: 
## http://chave.ups-tlse.fr/pantropical_allometry/E.nc.zip

if (!("E.nc" %in% list.files("data/GIS"))) {
  utils::download.file(
    url      = "http://chave.ups-tlse.fr/pantropical_allometry/E.nc.zip", 
    destfile = "data/GIS/E.nc.zip"
  )
  utils::unzip(
    zipfile = "data/GIS/E.nc.zip",
    files   = "E.nc", 
    exdir   = "data/GIS"
  )
  unlink("data/GIS/E.nc.zip")
}

envir_stress <- rast("data/GIS/E.nc")

## Data Preparations ----


## --- Prepare plot 
plot_init

plot <- plot_init %>%
  mutate(
    forest_type = case_when(
      site == "PGA" ~ "Dense Natural Forest",
      plot_id %in% c("PMT01", "PMT02", "PMT06") ~ "Dense Natural Forest",
      plot_id %in% c("PMT03", "PMT05", "PMT05") ~ "Dense Natural Forest",  
      plot_id %in% c("PSL01", "PSL02", "PSL03") ~ "Dense Natural Forest",
      plot_id %in% c("PSL04", "PSL05")          ~ "Dense Plantation Forest",
      )
    )

## --- Prepare sapling to merge with tree
sapling_init

sapling <- sapling_init %>% 
  mutate(
    sapling_id = paste0(sapling_id, "sa"),
    sapling_no = sapling_no * 100
    )

names(sapling) <- names(tree_init)

## --- Prepare tree
tree_init

tree <- tree_init %>%
  bind_rows(sapling) %>%
  mutate(
    tree_dbh_cor = round(tree_girth / pi, 0),
    tree_ba_cor  = round((tree_dbh / 200)^2 ),
    tree_volume  = round(tree_ba_cor * tree_height_top)
  ) %>%
  left_join(plot, by = "plot_id")


## Number of trees per plot 
tree %>%
  filter(tree_dbh >= 10) %>%
  group_by(plot_id) %>%
  summarise(count = n())


## Visual checks

tree %>%
  ggplot(aes(x = tree_dbh, y = tree_height_top)) +
  geom_point()

tree %>%
  ggplot(aes(x = tree_dbh, y = tree_height_top)) +
  geom_point(aes(color = site, shape = site)) +
  scale_shape_manual(values = c(1, 3, 2)) +
  theme_bw() +
  labs(x = "DBH (cm)", y = "Height (m)")




## Spatial data ----

sf_plot <- plot %>%
  filter(!is.na(plot_x), !is.na(plot_y)) %>%
  st_as_sf(coords = c("plot_x", "plot_y"))

sf_plot

st_crs(sf_plot) <- 4326

sf_plot_utm <- st_transform(sf_plot, 32751)

## + Admin ----

sf_district <- st_read("data/GIS/Districts/13districts.shp")

sf_country <- sf_district %>% summarise()

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


## Get plot data on forest type and E

sf_plot

plot_jica <- terra::extract(rs_jica, vect(sf_plot_utm))
plot_jica


plot_E <- terra::extract(envir_stress, vect(sf_plot), buffer = 1800, fun = mean)
plot_E

plot2 <- as_tibble(sf_plot) %>%
  bind_cols(plot_jica, plot_E) %>%
  select(-plot_srs, -geometry, -starts_with("ID"), envir_stress = layer)
plot2

tree2 <- tree_init %>%
  bind_rows(sapling) %>%
  left_join(species_init, by = "species_vernacular_name") %>%
  left_join(plot2, by = "plot_id") %>%
  filter(!is.na(lc), !is.na(tree_dbh)) %>%
  mutate(
    tree_height_model = exp(0.893 - envir_stress + 0.760 * log(tree_dbh) - 0.0340 * (log(tree_dbh))^2),
    tree_height_error = exp(rnorm(n = dim(.)[1], mean = 0, sd = 0.243)),
    tree_height_sim   = tree_height_model * tree_height_error
    )


tree2 %>%
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height_top, shape = site), col = "darkgreen") +
  geom_point(aes(y = tree_height_sim, shape = site), col = "darkorange") +
  scale_shape_manual(values = c(1, 3, 2)) +
  labs(x = "DBH (cm)", y = "Height (m)", caption = "Green: measurments\nYellow: model")

tree2

tree3 <- tree2 %>%
  select(-plot_date, -forest_type, -forest_status, -site, -lc, -envir_stress, -natural_planted)

dir.create("results")
write_csv(tree3, "results/tree.csv")
write_csv(plot2, "results/plot.csv")


## Calc tree density for measurement timing

tree_density <- tree2 %>%
  mutate(
    dbh_class2 = if_else(tree_dbh < 30, "less30", "more30"),
    dbh_class = floor(tree_dbh/10) * 10,
    tree_density = 100^2/if_else(tree_dbh < 30, 10 * 10, 20 * 25)
    ) %>%
  group_by(dbh_class, plot_id) %>%
  summarise(count = sum(tree_density))
tree_density

tree_density2 <- tree_density %>%
  group_by(dbh_class) %>%
  summarise(count = mean(count))
tree_density2

seedling_density <- seedling_init %>%
  group_by(plot_id) %>%
  summarise(count = n() * 100^2/5^2) %>%
  summarise(count = mean(count))
seedling_density

tree_density3 <- tree_density2 %>%
  bind_rows(list(dbh_class = 0, count = seedling_density$count)) %>%
  arrange(dbh_class) %>%
  mutate(dbh_class2 = dbh_class + 5)
tree_density3

ggplot(tree_density3) +
  geom_point(aes(x = dbh_class2, y = count))

(1667 + 175) / 2

##  0-10 => 1500 trees/ha
## 10-30 => 1000 trees/ha
## 30+   =>  290 trees/ha

# Modelling to get trees 10 cm dbh but not working well with no data point
# data_model <- tree_density3 %>% 
#   select(-dbh_class) %>%
#   filter(dbh_class2 != 15)
# 
# ggplot(data_model) +
#   geom_point(aes(x = dbh_class2, y = count))
# 
# ggplot(data_model) +
#   geom_text(aes(x = dbh_class2, y = 1 + 2000/exp(count), label = dbh_class2))
# 
# tt <- tibble(x = 1:100, y = 2000 * (1:100)^-0.8)
# 
# ggplot(tt, aes(x =x , y=y)) + 
#   geom_line() +
#   geom_point(data = data_model, aes(x = dbh_class2, y = count))
# 
# lm1 <- lm(data = data_model, formula = log(count) ~ log(dbh_class2))
# summary(lm1)
# coef(lm1)
# 
# tt <- tibble(x = 1:100, y = exp(coef(lm1)[1]) * x^coef(lm1)[2])
# 
# ggplot(data_model, aes(x = dbh_class2)) + 
#   geom_point(aes(y = count)) +
#   geom_line(data = tt, aes(x = x , y = y)) +
#   xlim(0, 100) + ylim(0, 2000)
# 
# 
# library(lmfor)
# 
# startHDweibull(d = data_model$dbh_class2, h = data_model$count, bh = 0)
# 
# nlme1 <- nlme(
#   model = count ~ a * (1 - exp(-b * dbh_class2^c)),
#   data = cbind(data_model, g = "a"),
#   fixed = a + b + c ~ 1,
#   groups = ~g,
#   #start = c(a = 1000, b = 10, c = -1),
#   start = c(a = 1600, b = 50000, c = -10)
# )
# 
# summary(nlme1)
# 
# 
# nlme2 <- nlme(
#   model = count ~ a * (1 - exp(- dbh_class2^b)),
#   data = cbind(data_model, g = "a"),
#   fixed = a + b ~ 1,
#   groups = ~g,
#   start = c(a = 2000, b = -1)
# )

summary(nlme2)


tt <- tibble(x = 1:100, y = fixef(nlme2)[1] * (1 - exp(-x^fixef(nlme2)[2])))

ggplot(data_model, aes(x = dbh_class2)) + 
  geom_point(aes(y = count)) +
  geom_line(data = tt, aes(x = x , y = y)) +
  xlim(0, 100) + ylim(0, 2000)





