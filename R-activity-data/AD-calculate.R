
library(tidyverse)

## FRL period: 15 October 2016 - 15 October 2021 

## FRL period based on AD: 1st January 2017 to 31 December 2021



##
## Load data ###################################################################
##

## Add value of cells
dggrid_table <- read_csv("data/GIS/dggrid_resolutions.csv")
dggrid_table
dggrid_table %>% filter(res == 15) 
area_cell <- dggrid_table %>% filter(res == 15) %>% pull(area_km) 

## Add CE points
ce_points <- read_csv("results/CE_points_clean.csv")

ad_points <- ce_points %>%
  mutate(
    lu_change_year = if_else(lu_change_year>= 2017, lu_change_year, NA_real_),
    redd_activity  = if_else(is.na(lu_change_year), "No change", redd_activity),
    lu_cat_old_fct = if_else(is.na(lu_change_year), lu_cat_new_fct, lu_cat_old_fct)
  )

table(ad_points$redd_activity, ad_points$lu_change_year, useNA = "ifany")

table(ad_points$lu_cat_old_fct, ad_points$lu_cat_new_fct, ad_points$lu_change_year, useNA = "ifany")



##
## Annual activity calculations ################################################
##

table(ad_points$lu_sub_new)

ad_annual <- ad_points %>%
  filter(!is.na(lu_change_year)) %>%
  group_by(lu_change_year, redd_activity) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  mutate(
    ad_area = count * area_cell * 100,
    ad_area_graph = if_else(redd_activity %in% c("AF", "E-FF"), -ad_area, ad_area)
   )
ad_annual

ggplot(ad_annual) +
  geom_col(aes(x = lu_change_year, y = ad_area_graph, fill = redd_activity)) +
  scale_fill_manual(values = c("green", "red", "grey80", "grey20")) +
  scale_x_continuous(breaks = 2017:2022)


