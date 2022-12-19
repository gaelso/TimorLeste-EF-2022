
library(tidyverse)

#file_path <- "data/Activity data/Export_results_all"
file_path <- "data/Activity data/Activity Data Results"

## Load data
ad_files <- list.files(path = file_path, pattern = ".csv", recursive = T, full.names = T)

survey_name <- ad_files %>%
  str_remove(".*/") %>%
  str_remove("_.*") %>%
  unique()

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

names(tt)

str_subset(string = names(tt), pattern = "year")
str_subset(string = names(tt), pattern = "actively_saved")
str_subset(string = names(tt), pattern = "file")


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
  filter(!is.na(id), !is.na(group))

write_csv(tt2, "results/all_data.csv")

table(tt2$duplicate_records)
table(tt2$group_chr, useNA = "ifany")
table(tt2$group, useNA = "ifany")

## Check duplicates again
test <- tt2 %>%
  group_by(id, group) %>%
  summarise(count = n())

table(test$group, test$count)

test2 <- tt2 %>%
  distinct(id, group, .keep_all = T)
  
## Check ids and groups
test <- tt2 %>%
  select(id, group) %>%
  distinct(id, group) %>%
  group_by(id) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1)

table(test$count)

vec <- test %>% 
  filter(count > 1) %>%
  pull(id) %>% 
  unique()  

id_group_dup <- tt2 %>% 
  filter(id %in% vec) %>%
  distinct(id, group, .keep_all = T) %>%
  select(id, group, operator, plot_file, file_name, survey_date) %>%
  arrange(id)

write_csv(id_group_dup, "results/id_group_duplicates.csv")


max_date <- tt2 %>%
  group_by(id) %>%
  summarise(max_date = max(survey_date))

tt3 <- tt2 %>%
  left_join(max_date, by = "id") %>%
  filter(!(duplicate_records & survey_date != max_date)) %>%
  select(-survey_month, -survey_day, -survey_date, -duplicate_records, -max_date) %>%
  arrange(id) %>%
  distinct(id, .keep_all = T)
tt3


## Check for duplicates
test <- tt3 %>%
  group_by(id) %>%
  summarise(count = n()) 

table(test$count)

vec <- test %>%
  filter(count > 1) %>%
  pull(id)

write_csv(tt3, paste0("results/sbae_2km_TL_clean_", Sys.Date(), ".csv"))



##
## Compa with original grid
##

sbae_original <- read_csv(list.files("data/Activity data/Original grid", full.names = T))
sbae_original

## Find operators for missing points
ce_files <- list.files(
  path = "data/Activity data/Original grid/TimorLeste_points_hex_2km_CE.csv_8_random/div_8", 
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
      group = group,
      id = as.numeric(id)
      )
  
}) %>%
  arrange(id)

ce_points

## Check problems with group
test <- ce_points %>%
  filter(id %in% unique(id_group_dup$id)) 





tt5 <- tt3 %>%
  select(id, operator) %>%
  full_join(ce_points, by = "id") %>%
  arrange(id)
tt5

table(tt5$operator, tt5$group, useNA= "ifany")

list_missing_ids <- tt5 %>%
  filter(is.na(operator)) %>%
  select(-operator, group, id, everything()) %>%
  arrange(group, id)

write_csv(list_missing_ids, "results/list_missing_ids.csv")


## Check again what point from which group
tt 

tt$plot_file %>% 
  str_remove(".*_") %>%
  str_remove(" \\(1\\).csv") %>%
  str_remove(".csv")
  
tt$plot_file %>% str_detect("Aileu")

list.files(path = file_path, pattern = "Aileu", recursive = T, full.names = T)

test <- tt %>%
  mutate(check_file = if_else(str_detect(plot_file, "Aileu"), 1, 0))

table(test$check_file)

test2 <- test %>% filter(check_file == 1)
write_csv(test2, "results/check_plot_file_errors.csv")

