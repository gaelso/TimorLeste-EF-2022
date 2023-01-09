
library(tidyverse)

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

