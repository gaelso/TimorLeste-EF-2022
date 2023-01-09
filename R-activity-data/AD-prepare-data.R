
library(tidyverse)
library(sf)

## Read clean data
list.files("results", pattern = "sbae_2km")

tt <- read_csv("results/sbae_2km_TL_clean_2023-01-05.csv")
tt

lu_conv <- read_csv("data/activity data/land_use_names.csv")
lu_conv

# ## Check Collect Earth table structure
# names(tt)
# 
# head(tt$land_use_category)
# head(tt$land_use_category_label)
# head(tt$land_use_category_has_changed)
# head(tt$land_use_subcategory)
# head(tt$land_use_subcategory_label)
# head(tt$land_use_subcategory_year_of_change)
# head(tt$land_use_subcategory_year_of_change_label)
# head(tt$land_use_subdivision)
# head(tt$land_use_subdivision_label)
# head(tt$land_use_subdivision_change)
# head(tt$land_use_subdivision_year_of_change)
# head(tt$land_use_subdivision_year_of_change_label)
# head(tt$land_use_initial_subdivision_label)
# head(tt$land_use_subdivision_year_of_change)
# 
# table(tt$land_use_subcategory_label, tt$land_use_subcategory_year_of_change_label)
# table(tt$land_use_initial_subdivision_label, tt$land_use_subdivision_label) 
# 
# table(tt$land_use_initial_subdivision_label, tt$land_use_subdivision_label) 


## Create id tables for land use categories and division
lu_cat <- tt %>%
  select(lu_cat = land_use_category_label) %>%
  distinct() %>%
  mutate(lu_cat_id = c(6, 1, 3, 5, 4, 2)) %>%
  arrange(lu_cat_id)

lu_cat_sub <- tt %>%
  select(lu_cat = land_use_category_label, lu_sub = land_use_subdivision_label) %>%
  distinct() %>%
  arrange(lu_cat)
lu_cat_sub

## Remove the detailed percentages and add to the data:
## + Main category of origin
## + Sorted categories (fct)
## + REDD Activity
## + Year of deforestation
## + Year of afforestation
ce_points <- tt %>%
  select(
    id,
    group,
    lu_cat_new     = land_use_category_label, 
    lu_change      = land_use_subcategory_label,
    lu_change_year = land_use_subcategory_year_of_change_label,
    lu_sub_new     = land_use_subdivision_label,
    lu_sub_old     = land_use_initial_subdivision_label,
    lu_sub_year    = land_use_subdivision_year_of_change_label,
    surveyor_name,
    plot_file, 
    file_name,
    survey_date,
    location_srs, 
    location_x, 
    location_y
    ) %>%
  left_join(lu_cat_sub, by = c("lu_sub_old" = "lu_sub")) %>%
  rename(lu_cat_old = lu_cat) %>%
  left_join(lu_cat, by = c("lu_cat_new" = "lu_cat")) %>%
  rename(lu_cat_new_id = lu_cat_id) %>%
  left_join(lu_cat, by = c("lu_cat_old" = "lu_cat")) %>%
  rename(lu_cat_old_id = lu_cat_id) %>%
  mutate(
    lu_cat_new_fct = forcats::fct_reorder(lu_cat_new, lu_cat_new_id),
    lu_cat_old_fct = forcats::fct_reorder(lu_cat_old, lu_cat_old_id),
    redd_activity  = case_when(
      lu_cat_old == 'Forest' & lu_cat_new != "Forest" ~ "DF",
      lu_cat_old != 'Forest' & lu_cat_new == "Forest" ~ "AF",
      lu_cat_new == lu_cat_old                        ~ "No change",
      TRUE ~ "Other"
    ),
  )

ce_points

write_csv(ce_points, "results/CE_points_clean.csv")

table(ce_points$lu_cat_old_fct, ce_points$lu_cat_new_fct)
table(ce_points$lu_cat_old_fct, ce_points$lu_cat_new_fct, ad_point$lu_change_year)
table(ce_points$lu_change)
table(ce_points$redd_activity, ce_points$lu_change_year)






