
library(terra)
library(sf)
library(tidyverse)

sf_country <- sf::st_read("data/GIS/TimorLeste.geoJSON") 
sf_country_wgs84 <- sf_country %>% sf::st_transform(crs = 4326)



##
## Precipitation ###############################################################
##

list_pp <- list.files("data/GIS/wc2.1_30s_prec", pattern = ".tif")

df_dry <- purrr::map_dfc(seq_along(list_pp), function(x){
  
  rs <- terra::rast(file.path("data/GIS/wc2.1_30s_prec", list_pp[x]))
  rs_crop <- terra::crop(rs, vect(sf_country_wgs84))
  rs_mask <- terra::mask(rs_crop, vect(sf_country_wgs84))
  #plot(rs_mask) 
  names(rs_mask) <- "pp"
  
  df<- terra::as.data.frame(rs_mask, xy = TRUE) %>%
    as_tibble() 
  
  df$growing_period <- if_else(df[3] > 60, 1, 0)
  
  names(df)[3:4] <- c(
    paste0("pp", if_else(x < 10, "0", ""), x),
    paste0("growing_period", if_else(x < 10, "0", ""), x)
  )
  
  if (x == 1) df else df[3:4]
  

})

df_dry

df_pp_tot <- df_dry %>%
  rowwise() %>%
  mutate(
    pp_tot = sum(c_across(starts_with("pp"))),
    LGP = sum(c_across(starts_with("growing_period")))
    )

df_pp_txt <- df_pp_tot %>%
  rowwise() %>%
  mutate(
    LGP_txt = paste0(c_across(starts_with("growing_period")), collapse = "")
  )

head(df_pp_txt$LGP_txt)
unique(df_pp_txt$LGP_txt)



df_pp <- df_pp_txt %>%
  mutate(
    pp_class = case_when(
      pp_tot < 1000 ~ 1,
      pp_tot < 1500 ~ 2,
      pp_tot < 2000 ~ 3,
      TRUE ~ 4
      ),
    pp_class_f = factor(
      pp_class,
      levels = c("<1000mm", "1000-1500mm", "1500-2000mm", ">= 2000mm")
      )#,
    # LGP_class = case_when(
    #   LGP_txt ==  ~ "<=5",
    #   LGP ==  ~ "6",
    #   LGP == 7 ~ "7",
    #   LGP ==
    # )
    )

ggplot(df_pp) +
  geom_tile(aes(x = .data$x, y = .data$y, fill = .data$pp_tot)) +
  scale_fill_viridis_c() +
  geom_sf(data = sf_country_wgs84, fill = NA, col = "darkred", size = 1) +
  theme(legend.position = "bottom") +
  theme_void()

ggplot(df_pp) +
  geom_tile(aes(x = .data$x, y = .data$y, fill = .data$pp_class)) +
  scale_fill_viridis_c() +
  geom_sf(data = sf_country_wgs84, fill = NA, col = "darkred", size = 1) +
  theme(legend.position = "bottom") +
  theme_bw()

ggplot(df_pp) +
  geom_tile(aes(x = .data$x, y = .data$y, fill = .data$LGP)) +
  scale_fill_viridis_c(direction = -1) +
  geom_sf(data = sf_country_wgs84, fill = NA, col = "darkred", size = 1) +
  theme(legend.position = "bottom") +
  theme_void()

ggplot(df_pp) +
  geom_tile(aes(x = .data$x, y = .data$y, fill = .data$LGP_txt)) +
  scale_fill_viridis_d() +
  geom_sf(data = sf_country_wgs84, fill = NA, col = "darkred", size = 1) +
  theme(legend.position = "bottom") +
  theme_void()



##
## Convert back to raster and save data.
##
rs_pp_tot <- terra::rast(df_pp %>% dplyr::select("x", "y", "pp_tot"))
plot(rs_pp_tot)

terra::writeRaster(rs_pp_tot, "data/GIS/total_pp_fromWorldClim.tif")


rs_pp_class <- terra::rast(df_pp %>% dplyr::select("x", "y", "pp_class"))
plot(rs_pp_class)

terra::writeRaster(rs_pp_class, "data/GIS/pp_class_fromWorldClim.tif")



################################################################################
################################################################################
################################################################################

ggplot(df_dry) +
  geom_tile(aes(x = .data$x, y = .data$y, fill = .data$nb_dry)) +
  scale_fill_viridis_c(direction = -1) +
  geom_sf(data = sf_country_wgs84, fill = NA, col = "darkred", size = 1) +
  theme(legend.position = "bottom") +
  theme_bw()

# rs_dry <- terra::rast(df_dry %>% dplyr::select("x", "y", "nb_dry"))
# plot(rs_dry)



##
## Temperature #################################################################
##


list_tavg <- list.files("data/GIS/wc2.1_30s_tavg", pattern = ".tif")

df_tavg <- purrr::map_dfc(seq_along(list_tavg), function(x){
  
  rs <- terra::rast(file.path("data/GIS/wc2.1_30s_tavg", list_tavg[x]))
  rs_crop <- terra::crop(rs, vect(sf_country_wgs84))
  rs_mask <- terra::mask(rs_crop, vect(sf_country_wgs84))
  #plot(rs_mask) 
  names(rs_mask) <- paste0("t", if_else(x < 10, "0", ""), x) 
  
  df<- terra::as.data.frame(rs_mask, xy = TRUE) %>%
    as_tibble() 
  
  if (x == 1) df else df[3]
  
})

df_tavg

df_tavg$tavg <- rowSums(df_tavg[3:14]) / 12
df_tavg$mountain <- if_else(df_tavg$tavg < 20, "Highland", "Lowland")

ggplot(df_tavg) +
  geom_tile(aes(x = .data$x, y = .data$y, fill = .data$tavg)) +
  scale_fill_viridis_c() +
  geom_sf(data = sf_country_wgs84, fill = NA, col = "darkred", size = 1) +
  theme(legend.position = "bottom") +
  theme_bw()

ggplot(df_tavg) +
  geom_tile(aes(x = .data$x, y = .data$y, fill = .data$mountain)) +
  geom_sf(data = sf_country_wgs84, fill = NA, col = "darkred", size = 1) +
  scale_fill_manual(values = c("black", "white")) +
  theme_void() + 
  theme(legend.position = "bottom") +
  labs(fill = "")
