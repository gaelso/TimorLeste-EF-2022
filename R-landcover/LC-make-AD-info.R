
library(tidyverse)
library(readxl)
library(sf)

## Matching table between NFI plot ID and SBAE ID 
id_match <- read_csv("data/NFItest/AD_NFI_IDmatch.csv")


## Loading AD data
#ad_concat <- read_csv("results/all_data.csv")

# ad_clean <- read_csv("results/sbae_2km_TL_clean_2022-12-14.csv")
ad_clean <- read_csv("data/Activity data/AD FRL/Copy of Final_AD_Timor-Leste.csv")


## Prepare LC from AD data
ad_clean_nfi <- ad_clean %>%
  filter(id %in% id_match$ad_point_id) %>%
  select(id, ad_location_x = location_x, ad_location_y = location_y, land_use_category_label, land_use_subdivision_label)


## Loading NFI plot data
plot <- read_xlsx("data/NFItest/TL_NFI_DATA_202300601.xlsx", sheet = "plot") %>%
  mutate(
    plot_center_x = round(as.numeric(plot_center_GPS_E), 4),
    plot_center_y = -round(as.numeric(plot_center_GPS_S), 4),
    plot_center_elevation = round(as.numeric(plot_center_elevation))
    )
# write_csv(plot, "results/nfitest_plot.csv")

lf_object <- read_xlsx("data/NFItest/TL_NFI_DATA_202300601.xlsx", sheet = "land_feature_object") %>%
  mutate(
    lf_artif = case_when(
      lf_object_artificiality == "Plantation" ~ "Artificial",
      lf_object_artificiality == "Plantation coffe" ~ "Artificial",
      TRUE ~ lf_object_artificiality
    )
  ) 

## Prepare artificiality
table(lf_object$lf_object_artificiality)
table(lf_object$lf_artif)

plot_is_artif <- lf_object %>% 
  filter(lf_artif == "Artificial") %>%
  pull(plot_no)

plot_is_nat <- lf_object %>% 
  filter(lf_artif == "Natural") %>%
  pull(plot_no)

## Update plot level info
plot2 <- plot %>% 
  select(plot_no, plot_center_x, plot_center_y, plot_center_elevation) %>%
  mutate(
    lc_artif = if_else(plot_no %in% plot_is_artif, "Artificial", "Natural"),
    lc_elevation = case_when(
      plot_center_elevation <= 600 ~ "Lowland",
      plot_center_elevation <= 1000 ~ "Highland",
      plot_center_elevation > 1000 ~ "Mountain",
      TRUE ~ NA_character_
      )
    ) %>%
  left_join(id_match) %>%
  left_join(ad_clean_nfi, by = c("ad_point_id" = "id"))

plot2  

## Convert coordinates to metric

sf_plot_nfi <- plot2 %>%
  filter(!is.na(plot_center_x), !is.na(plot_center_y)) %>%
  select(plot_no, plot_center_x, plot_center_y) %>%
  st_as_sf(coords = c("plot_center_x", "plot_center_y"), crs = 4326) %>%
  st_transform("ESRI:54017") %>%
  mutate(
    nfi_x = st_coordinates(.)[,1],
    nfi_y = st_coordinates(.)[,2]
    ) %>%
  as_tibble() %>%
  select(-geometry)

sf_plot_ad <- plot2 %>%
  select(plot_no, ad_location_x, ad_location_y) %>%
  filter(!is.na(ad_location_x)) %>%
  st_as_sf(coords = c("ad_location_x", "ad_location_y"), crs = 4326) %>%
  st_transform("ESRI:54017") %>%
  mutate(
    ad_x = st_coordinates(.)[,1],
    ad_y = st_coordinates(.)[,2]
  ) %>%
  as_tibble() %>%
  select(-geometry)

plot3 <- plot2 %>%
  left_join(sf_plot_nfi, by = "plot_no") %>%
  left_join(sf_plot_ad, by = "plot_no") %>%
  mutate(
    distance_ad_nfi = sqrt((nfi_x - ad_x)^2 + (nfi_y - ad_y)^2),
    lf_landcover = if_else(lc_artif == "Artificial", "Forest plantation", target_lc_name)
  ) %>%
  rename(nfi_landcover = target_lc_name, ad_landcover = land_use_subdivision_label) %>%
  select(plot_no, zone, nfi_landcover, lf_landcover, ad_landcover, distance_ad_nfi, target_climate)
  
summary(plot3$distance_ad_nfi)

write_csv(plot3, "results/nfi_plot_lc.csv")
