
library(tidyverse)
library(googledrive)


download_data <- FALSE


if (download_data) {
  
  drive_find(pattern = "Actity_Data", n_max = 100)
  drive_ls(path = "Actity_Data", type = "folder")
  path_to_files <- drive_ls(path = "Actity_Data/Export_results_all_versions", type = "csv")
  
  
  dir.create("data/activity data", showWarnings = F)
  dir.create("data/activity data/AD all", showWarnings = F)
  
  unlink(list.files(path = "data/activity data/AD all", full.names = T))
  
  purrr::walk(seq_along(path_to_files$id), function(x){
    
    drive_download(file = path_to_files$id[x], 
                   path = file.path("data/activity data/AD all", path_to_files$name[x]), 
                   overwrite = T
    )
    
  })
  
}


##
## Load data in R ######
##

#file_path <- "data/activity data/Export_results_all"
file_path <- "data/activity data/AD all"

## List files data
ad_files <- list.files(path = file_path, pattern = ".csv", recursive = T, full.names = T)
ad_files

survey_name <- ad_files %>%
  str_remove(".*/") %>%
  str_remove("_.*") %>%
  unique()

## Load and combine
tt <- map_dfr(ad_files, function(x){
  
  surveyor_name <- x %>%
    str_remove(".*/") %>%
    str_remove("_.*")
  
  file_name <- x %>%
    str_remove(".*/")
  
  tt <- read_csv(x, col_types = list(.default = "c")) %>%
    mutate(
      surveyor_name = surveyor_name,
      file_name = file_name
      )
  
})

# names(tt)
# str_subset(string = names(tt), pattern = "year")
# str_subset(string = names(tt), pattern = "actively_saved")
# str_subset(string = names(tt), pattern = "file")


## Check ID
test <- tt %>%
  group_by(id) %>%
  summarise(count = n())

table(test$count)

vec <- test %>%
  filter(count > 1) %>%
  pull(id)

test2 <- tt %>%
  filter(id %in% vec) %>%
  arrange(id)

test3 <- test2 %>% 
  select(
    id, operator, surveyor_name, actively_saved, 
    actively_saved_on_year, actively_saved_on_month, actively_saved_on_day
    )

## Solve issues with data format and add info
tt2 <- tt %>%
  mutate(
    id = as.numeric(id),
    survey_month = if_else(
      as.numeric(actively_saved_on_month) < 10, 
      paste0("0", actively_saved_on_month),
      actively_saved_on_month),
    survey_day = if_else(
      as.numeric(actively_saved_on_day) < 10, 
      paste0("0", actively_saved_on_day),
      actively_saved_on_day),
    survey_date = as.numeric(paste0(actively_saved_on_year, survey_month, survey_day)),
    duplicate_records = if_else(id %in% vec, TRUE, FALSE),
    group_chr = plot_file %>% str_remove(".*_") %>% str_remove(" \\(1\\).csv") %>% str_remove(".csv"),
    group = as.numeric(group_chr)
    ) %>%
  select(id, group, group_chr, operator, surveyor_name, plot_file, file_name, everything())

write_csv(tt2, "results/all_data.csv")

table(tt2$duplicate_records)
table(tt2$group_chr, useNA = "ifany")
table(tt2$group, useNA = "ifany")

##
## Checks on combined data to find and remove duplicates
##

## Check wrong IDs or group not from 1 to 8
test <- tt %>% 
  mutate(id_num = as.numeric(id)) %>%
  filter(is.na(id_num)) %>%
  select(id, id_num)

## Check wrong group
table(tt2$group_chr, tt2$group, useNA = "ifany")

## Check date
table(tt2$survey_date, tt2$group_chr, useNA = "ifany")

## Add max date to keep only latest record for duplicates
max_date <- tt2 %>%
  group_by(id) %>%
  summarise(max_date = max(survey_date))

## Check duplicates based on group
tt2 %>% distinct(id, .keep_all = T) %>% nrow()
tt2 %>% distinct(id, group, .keep_all = T) %>% nrow()

test <- tt2 %>%
  filter(!is.na(id), !is.na(group)) %>%
  #  distinct(id, group, .keep_all = T) %>%
  group_by(id, group) %>%
  summarise(count = n())

table(test$group, test$count)

test2 <- tt2 %>%
  distinct(id, group, .keep_all = T)

## Check ids and groups
test <- tt2 %>%
  filter(!is.na(id), !is.na(group)) %>%
  select(id, group) %>%
  distinct(id, group) %>%
  group_by(id) %>%
  summarise(count = n(), .groups = "drop")

table(test$count)

vec <- test %>% 
  filter(count > 1) %>%
  pull(id) %>% 
  unique()  

id_group_dup <- tt2 %>%
  filter(!is.na(id), !is.na(group)) %>%
  filter(id %in% vec) %>%
  distinct(id, group, .keep_all = T) %>%
  select(id, group, operator, plot_file, file_name, survey_date) %>%
  arrange(id)
id_group_dup

write_csv(id_group_dup, "results/id_group_duplicates.csv")

file_rm <- tt2 %>% 
  filter(id %in% vec) %>%
  filter(group == 5) %>%
  pull(file_name) %>%
  unique()

## Cross check group to remove
test <- tt2 %>% 
  filter(group == 5) %>%
  select(operator, plot_file, file_name) %>%
  distinct()

## Remove missing IDs and group plus keep only max date for records
## And use distinct() fro further removing duplicates
tt3 <- tt2 %>%
  filter(!is.na(id), !is.na(group)) %>%
  filter(!(group == 5 & file_name %in% file_rm)) %>%
  #left_join(max_date, by = "id") %>%
  #filter(survey_date == max_date) %>%
  select(-survey_month, -survey_day, -duplicate_records) %>%
  arrange(id, desc(survey_date)) %>%
  distinct(id, .keep_all = T)
tt3 

write_csv(tt3, paste0("results/sbae_2km_TL_clean_", Sys.Date(), ".csv"))

tt3b <- tt3 %>%
  mutate(
    lu_cat_new = str_sub(land_use_category_label, 0, 1), 
    lu_change_code = if_else(is.na(land_use_subcategory), paste0(lu_cat_new, lu_cat_new), land_use_subcategory),
    lu_sub_new = if_else(is.na(land_use_subdivision_label), str_sub(lu_change_code, 2, 2), land_use_subdivision_label),
    lu_sub_new = case_when(
      lu_sub_new == "C"                        ~ "Cropland",
      lu_sub_new == "O"                        ~ "Other Land",
      lu_sub_new == "Moist high land forest"   ~ "Moist Highland forest",
      lu_sub_new == "Infrastructure"           ~ "Settlements",
      lu_sub_new == "Settlement"               ~ "Settlements",
      lu_sub_new == "Lakes/Lagoons/Reservoirs" ~ "Wetlands",
      lu_sub_new == "River"                    ~ "Wetlands",
      lu_sub_new == "Mining"                   ~ "Other Land",
      lu_sub_new == "Other bareland"           ~ "Other Land",
      lu_sub_new == "Rocks"                    ~ "Other Land",
      lu_sub_new == "Sand"                     ~ "Other Land",
      TRUE ~ lu_sub_new
      ),
    lu_sub_new = str_to_title(lu_sub_new),
    lu_change_year = if_else(!is.na(land_use_subdivision_year_of_change_label), land_use_subdivision_year_of_change_label, land_use_subcategory_year_of_change_label),
    lu_sub_old = case_when(
    is.na(lu_change_year) ~ lu_sub_new,
    is.na(land_use_initial_subdivision_label) ~ str_sub(lu_change_code, 0, 1),
    TRUE ~ land_use_initial_subdivision_label
      ),
    lu_sub_old = case_when(
      lu_sub_old == "F" ~ lu_sub_new, ## Error change since no initial LU reported 
      lu_sub_old == "G" ~ lu_sub_new, ## Error change since no initial LU reported
      lu_sub_old == "S" ~ lu_sub_new, ## Error change since no initial LU reported
      lu_sub_old == "O" ~ "Other Land",
      lu_sub_old == "C" ~ "Cropland",
      lu_sub_old == "Moist high land forest"   ~ "Moist Highland forest",
      lu_sub_old == "Infrastructure"           ~ "Settlements",
      lu_sub_old == "Settlement"               ~ "Settlements",
      lu_sub_old == "Lakes/Lagoons/Reservoirs" ~ "Wetlands",
      lu_sub_old == "River"                    ~ "Wetlands",
      lu_sub_old == "Mining"                   ~ "Other Land",
      lu_sub_old == "Other bareland"           ~ "Other Land",
      lu_sub_old == "Rocks"                    ~ "Other Land",
      lu_sub_old == "Sand"                     ~ "Other Land",
      TRUE ~ lu_sub_old
      ),
    lu_sub_old = str_to_title(lu_sub_old)
    )


table(tt3$land_use_subdivision_label, useNA = "ifany")
table(tt3$land_use_subcategory, tt3$land_use_subdivision_label, useNA = "ifany")
table(tt3$land_use_subcategory, useNA = "ifany")

table(tt3b$lu_cat_new, useNA = "ifany")
table(tt3b$lu_change_code, useNA = "ifany")
table(tt3b$lu_sub_new, useNA = "ifany")
table(tt3b$lu_change_year, useNA = "ifany")

table(tt3b$lu_sub_old, useNA = "ifany")

## Check removed IDs
tt2 %>%
  filter(!is.na(id)) %>%
  pull(id) %>%
  unique() %>%
  length()

## Check for duplicates
test <- tt3 %>%
  group_by(id) %>%
  summarise(count = n()) 

table(test$count)

## If still has duplicates
# vec <- test %>%
#   filter(count > 1) %>%
#   pull(id)
# 
# test2 <- tt3 %>%
#   filter(id %in% vec) %>%
#   arrange(id) %>%
#   select(id, group, operator, surveyor_name, file_name, plot_file)
# 
# tt2 %>%
#   filter(group == 5) %>%
#   select(operator, surveyor_name, file_name, plot_file) %>%
#   distinct()


##
## Compa with original grid
##

# sbae_original <- read_csv(list.files("data/activity data/Original grid", full.names = T))
# sbae_original

## Find operators for missing points
ce_files <- list.files(
  path = "data/activity data/Original grid/TimorLeste_points_hex_2km_CE.csv_8_random/div_8", 
  pattern = ".csv", 
  full.names = T
  )

ce_group <- ce_files %>%
  str_remove(".*_") %>%
  str_remove("\\..*")

ce_points <- map_dfr(ce_files, function(x){
  
  group <- x %>%
    str_remove(".*_") %>%
    str_remove("\\..*") %>%
    as.numeric()
  
  tt <- read_csv(x, col_types = list(.default = "c")) %>%
    mutate(
      group      = group,
      id         = as.numeric(id),
      no         = 1:nrow(.),
      no_ingroup = case_when(
        no < 10   ~ paste0(group, "_000", no),
        no < 100  ~ paste0(group, "_00", no),
        no < 1000 ~ paste0(group, "_0", no),
        TRUE      ~ paste0(group, "_", no)
        )
      )
  
}) %>%
  select(id, group, no, no_ingroup, everything()) %>%
  arrange(id)

ce_points

test <- ce_points %>%
  mutate(no = 1:nrow(.)) %>%
  select(no, everything())

## Check problems with group
test <- ce_points %>%
  filter(id %in% unique(id_group_dup$id)) 


tt5 <- tt3 %>%
  select(id, operator, surveyor_name) %>%
  full_join(ce_points, by = "id") %>%
  arrange(id)
tt5

table(tt5$operator, tt5$group, useNA= "ifany")
table(tt5$surveyor_name, tt5$group, useNA= "ifany")

list_missing_ids <- tt5 %>%
  filter(is.na(surveyor_name)) %>%
  select(-operator, -surveyor_name) %>%
  select(group, no, everything()) %>%
  arrange(group, no)

list_missing_ids

write_csv(list_missing_ids, "results/list_missing_ids.csv")

# ## IF PLOTS STILL MISSING
# ## Check again if points in source files
# test <- tt2 %>% 
#   filter(id %in% list_missing_ids$id) %>%
#   arrange(id) %>%
#   distinct(id, .keep_all = TRUE)
# 
# write_csv(test, "results/missing ids found in orginal data.csv")
# 
# ## Check again what point from which group
# tt$plot_file %>% 
#   str_remove(".*_") %>%
#   str_remove(" \\(1\\).csv") %>%
#   str_remove(".csv")
#   
# tt$plot_file %>% str_detect("Aileu")
# 
# list.files(path = file_path, pattern = "Aileu", recursive = T, full.names = T)
# 
# test <- tt %>%
#   mutate(check_file = if_else(str_detect(plot_file, "Aileu"), 1, 0))
# 
# table(test$check_file)
# 
# test2 <- test %>% filter(check_file == 1)
# write_csv(test2, "results/check_plot_file_errors.csv")

