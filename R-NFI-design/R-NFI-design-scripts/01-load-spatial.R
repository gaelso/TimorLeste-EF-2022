
## +++ +++
## NFI design optimization scripts for Timor Leste
## +++ +++



## Load spatial data

## + Admin ----

sf_district <- st_read("data/GIS/Districts/13districts.shp")

sf_country <- sf_district %>% summarise()


## + JICA land cover ----

rs_jica_init <- rast("data/GIS/LUFC2012_Raster30_FEB20131/LUFC2012_Raster30_FEB20131.tif")
#summary(rs_jica_init)
#plot(rs_jica_init)
#levels(rs_jica_init)
#cats(rs_jica_init)

jica_code <- tibble(
  old_code = c(1, 3,  4,  5,  7,  9, 21, 22, 23, 27),
  new_code = c(0, 1, 9, 3, 4, 5, 6, 8, 7, 2)
)

# jica_lc <- tibble(
#   lc = c("Dense forest", "Sparse forest", "Unknown", "Grassland", "Bareland", 
#          "Inland Water", "Rice field", "Dry field", "Settlement", "Very sparse forest"),
#   hex = c("#009900", "#00ff00", "#ff0000", "#ffccff", "#cc00ff", 
#           "#0033ff", "#cccc00", "#ff9933", "#330000", "#99ff99")
# )

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

## Checks
# plot(rs_jica, col=jica_lc$hex)
# plot(sf_country, add=T)
# summary(rs_jica)
# cats(rs_jica)
# rs_jica

## !!! Not working
#rs_jica2 <- st_as_stars(rs_jica)

df_jica <- terra::as.data.frame(rs_jica, xy = TRUE) %>% 
  as_tibble() %>%
  na.omit()

area_ftype <- df_jica %>%
  group_by(lc) %>%
  summarise(count = n()) %>%
  mutate(
    area = count * 30^2 / 100^2,
    area_p = round(area / sum(area) * 100)
    ) %>%
  arrange(desc(area)) 
area_ftype

sum(area_ftype$area_p)

## + Avitabile 2016 Biomass map ----

rs_agb_init <- terra::rast("data/GIS/Avitabile_AGB_Map/Avitabile_AGB_Map.tif")
names(rs_agb_init) <- "agb_avitabile"
# summary(rs_agb_init)
# plot(rs_agb_init)
# levels(rs_agb_init)
# cats(rs_agb_init)

rs_agb_prepa1 <- terra::crop(rs_agb_init, ext(124, 128, -10, -8))
# plot(rs_agb_prepa1)

rs_agb_prepa2 <- terra::project(rs_agb_prepa1, "EPSG:32751", method = "near")
# plot(rs_agb_prepa2)

rs_agb_prepa3 <- mask(rs_agb_prepa2, vect(sf_country))
# plot(rs_agb_prepa3)
# plot(sf_country, add=T)

rs_agb <- crop(rs_agb_prepa3, rs_jica)
# plot(rs_agb)
# plot(sf_country, add=T)

df_agb <- terra::as.data.frame(rs_agb, xy = TRUE) %>% 
  as_tibble() %>%
  na.omit()

rs_jica100 <- resample(rs_jica, rs_agb)

df_jica100 <- as.data.frame(rs_jica100, xy = TRUE) %>% 
  as_tibble() %>%
  na.omit()


## + Checks ----

# rs_jica
# rs_agb
# 
# plot(rs_jica)
# plot(rs_agb)
 


## + Remove objects ----

rm(rs_jica_init, rs_agb_init, rs_agb_prepa1, rs_agb_prepa2, rs_agb_prepa3)

