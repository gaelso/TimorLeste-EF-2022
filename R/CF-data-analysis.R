## Metadata

## Community forestry data analysis
## GCF project: 
## REDD+ Readiness Support to Timor-Leste: Capacity and Institutional Development 
## April 2022


## Libraries ----

library(tidyverse)



## Data ----

plot_init <- read_csv("data/CF/plot.csv", show_col_types = FALSE)
tree_init <- read_csv("data/CF/tree.csv", show_col_types = FALSE)
sapling_init <- read_csv("data/CF/sapling.csv", show_col_types = FALSE)
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
  labs(x = "DBH (cm)", y = "Height (m)")

tree2

tree3 <- tree2 %>%
  select(-plot_date, -forest_type, -forest_status, -site, -lc, -envir_stress, -natural_planted)

dir.create("results")
write_csv(tree3, "results/tree.csv")
write_csv(plot2, "results/plot.csv")



