
library(tidyverse)

ad_files <- list.files(path = "data/Activity data", pattern = ".csv")

survey_name <- ad_files %>%
  str_remove("_.*")

tt <- map_dfr(ad_files, function(x){
  
  url <- file.path("data/Activity data", x)
  
  tt <- read_csv(url, col_types = list(.default = "c")) %>%
    mutate(surveyor_name = str_remove(x, "_.*"))
  
})

names(tt)

str_subset(string = names(tt), pattern = "year")

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

test2 %>% 
  select(
    id, operator, surveyor_name, actively_saved, 
    actively_saved_on_year, actively_saved_on_month, actively_saved_on_day
    )

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
    duplicate_records = if_else(id %in% vec, TRUE, FALSE)
    ) %>%
  filter(!is.na(id))

table(tt2$duplicate_records)

max_date <- tt2 %>%
  group_by(id) %>%
  summarise(max_date = max(survey_date))


tt3 <- tt2 %>%
  left_join(max_date, by = "id") %>%
  filter(!(duplicate_records & survey_date != max_date)) %>%
  select(-survey_month, -survey_day, -survey_date, -duplicate_records, -max_date) %>%
  arrange(id)
tt3

test <- tt3 %>%
  group_by(id) %>%
  summarise(count = n()) 

table(test$count)

write_csv(tt3, paste0("results/sbae_2km_TL_clean_", Sys.Date(), ".csv"))

## Compa with original grid
sbae_original <- read_csv(list.files("data/Activity data/original grid", full.names = T))
sbae_original

sbae_id
   