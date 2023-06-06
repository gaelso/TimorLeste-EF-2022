
library(sf)
library(tidyverse)

theme_set(theme_bw())

sf_aoi  <- st_read("data/spatial/TimorLeste.geoJSON")
sf_grid <- st_read("results/grid-ISEA3H-res15.geoJSON")

ce_data <- read_csv("results/sbae_2km_TL_clean_2022-12-14.csv")

table(ce_data$land_use_category_label)

lc_ipcc <- tibble(
  ipcc_cat = c("Forestland", "Grassland", "Cropland", "Wetlands", "Settlements", "Other lands"),
  ipcc_pb  = c(0.6         , 0.05       , 0.2       , 0.05      , 0.05         , 0.05         ), 
  ipcc_hex = c('#006400'   , '#ffff4c'  , '#f096ff' , '#0064c8' , '#fa0000'    , "#333333"    ),
  ipcc_no  = 1:6,
  ipcc_f   = fct_reorder(ipcc_cat, ipcc_no)
) %>%
  mutate(ipcc_cat = paste0("MOCK_", ipcc_cat))

n <- dim(sf_grid)[1]

set.seed(10)
sf_lc_mock <- sf_grid %>%
  mutate(
    lc2021 = sample(x = lc_ipcc$ipcc_f, size = n, replace = T, prob = lc_ipcc$ipcc_pb),
    lc2020 = sample(x = lc_ipcc$ipcc_f, size = n, replace = T, prob = lc_ipcc$ipcc_pb),
    lc2019 = sample(x = lc_ipcc$ipcc_f, size = n, replace = T, prob = lc_ipcc$ipcc_pb),
    lc2018 = sample(x = lc_ipcc$ipcc_f, size = n, replace = T, prob = lc_ipcc$ipcc_pb),
    lc2017 = sample(x = lc_ipcc$ipcc_f, size = n, replace = T, prob = lc_ipcc$ipcc_pb)
  )

st_write(sf_lc_mock, "results/MOCK_landcover_IPCC.geoJSON")

ggplot() +
  geom_sf(data = sf_aoi, fill = "grey90", color = 'black') +
  geom_sf(data = sf_lc_mock, aes(fill = lc2021), color = "grey70") +
  scale_fill_manual(values = lc_ipcc$ipcc_hex)

sf_nfi <- st_read("data/spatial/NFI TESTING_40plots.kml", layer = )

