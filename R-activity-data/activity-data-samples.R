
library(tidyverse)

ad_files <- list.files(path = "data/Activity data", pattern = ".csv")

survey_name <- ad_files %>%
  str_remove("_.*")

tt <- map_dfr(ad_files, function(x){
  
  url <- file.path("data/Activity data", x)
  
  tt <- read_csv(url, col_types = list(.default = "c")) %>%
    mutate(surevyor_name = str_remove(x, "_.*"))
  
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
    id, operator, surevyor_name, actively_saved, 
    actively_saved_on_year, actively_saved_on_month, actively_saved_on_day
    )


