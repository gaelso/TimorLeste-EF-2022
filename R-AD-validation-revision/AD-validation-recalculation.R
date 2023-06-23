
library(tidyverse)


## Relative paths
path_data <- "data/validation-revision"
dir.create(path_data, showWarnings = FALSE)

path_data2 <- file.path(path_data, "CE grid")
dir.create(path_data2, showWarnings = F)

path_data3 <- file.path(path_data, "CE results")
dir.create(path_data3, showWarnings = FALSE)

path_data4 <- file.path(path_data, "comparisons")
dir.create(path_data4, showWarnings = FALSE)

path_res <- file.path(path_data, "Valid new")
dir.create(path_res, showWarnings = FALSE)

##
## Load AD #####################################################################
##
ce_results <- read_csv(file.path(path_data3, "Results_calculated_Lorena_surveyy_v10.csv"))

table(ce_results$land_use_subcategory_year_of_change)

ce_AD_init <- ce_results %>%
  select(
    id, 
    lu_change      = land_use_subcategory, 
    lu_cat         = land_use_subdivision_label, 
    lu_cat_init    = land_use_initial_subdivision_label, 
    lu_change_year = land_use_subcategory_year_of_change,
    lu_sec_change      = second_lu_conversion, 
    lu_sec_change_cat  = second_lu_subdivision_label, 
    lu_sec_change_year = second_lu_conversion_year 
  ) %>%
  mutate(
    redd = case_when(
      lu_change %in% c("CF", "GF", "OF", "SF", "WF") ~ "AF",
      lu_change %in% c("FC", "FG", "FO", "FS", "FW") ~ "DF",
      lu_change == "FF" ~ "StableF",
      lu_change %in% c("CC", "GG", "OO", "SS", "WW") ~ "StableNF",
      TRUE ~ "StableNF"
    ),
    redd_FRL = case_when(
      !(lu_change_year %in% 2017:2021) & redd == "AF"      ~ "StableF",
      !(lu_change_year %in% 2017:2021) & redd == "DF"       ~ "StableNF",
      #!(lu_change_year %in% 2017:2021) & redd == "StableNF" ~ "StableNF",
      TRUE ~ redd
    )
  )

ce_AD <- ce_AD_init

names(ce_AD) <- names(ce_AD) %>% 
  paste0(., "_AD") %>%
  str_replace("id_AD", "id")

table(ce_AD$lu_sec_change_year_AD)

ce_AD_2changes <- ce_AD %>% filter(lu_sec_change_year_AD %in% 2017:2021)

## >>> Second LU Changes can be ignored as repeating the first LU change.


## Load initial validation
valid_points <- read_csv(file.path(path_data, "validation_allgroups_round2.csv"))

ce_valid <- valid_points %>%
  select(
    id, 
    lu_change = land_use_subcategory, 
    lu_cat = land_use_subdivision_label, 
    lu_cat_init = land_use_initial_subdivision_label, 
    lu_change_year = land_use_subcategory_year_of_change
  ) %>%
  mutate(
    redd = case_when(
      lu_change %in% c("CF", "GF", "OF", "SF", "WF") ~ "AF",
      lu_change %in% c("FC", "FG", "FO", "FS", "FW") ~ "DF",
      lu_change == "FF" ~ "StableF",
      lu_change %in% c("CC", "GG", "OO", "SS", "WW") ~ "StableNF",
      TRUE ~ "StableNF"
    ),
    redd_FRL = case_when(
      !(lu_change_year %in% 2017:2021) & redd == "AF" ~ "StableF",
      !(lu_change_year %in% 2017:2021) & redd == "DF" ~ "StableNF",
      !(lu_change_year %in% 2017:2021) & redd == "StableNF" ~ "StableNF",
      TRUE ~ redd
    )
  )


## For comparing valid with revision
ce_valid2 <- ce_valid

names(ce_valid2) <- names(ce_valid2) %>%
  paste0(., "_valid") %>%
  str_replace("id_valid", "id")


## Initial validation matrix ---------------------------------------------------

error_initial <- ce_AD %>% 
  filter(id %in% ce_valid$id) %>%
  left_join(ce_valid, by = "id") %>%
  group_by(redd_FRL_AD, redd_FRL) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = redd_FRL, values_from = count)

error_initial

(3+247+46)/425


##
## Revised validation points ###################################################
##

## Load IDs
path_rev <- "data/validation-revision/validation revision results"

path_rev_csv <- list.files(path_rev, recursive = TRUE, full.names = TRUE, pattern = ".csv")

rev_all_init <- map_dfr(path_rev_csv, read_csv)

rev_all <- rev_all_init %>%
  select(
    id, 
    lu_change      = land_use_subcategory, 
    lu_cat         = land_use_subdivision_label, 
    lu_cat_init    = land_use_initial_subdivision_label, 
    lu_change_year = land_use_subcategory_year_of_change,
    lu_sec_change      = second_lu_conversion, 
    lu_sec_change_cat  = second_lu_subdivision_label, 
    lu_sec_change_year = second_lu_conversion_year 
  ) %>%
  mutate(
    redd = case_when(
      lu_change %in% c("CF", "GF", "OF", "SF", "WF") ~ "AF",
      lu_change %in% c("FC", "FG", "FO", "FS", "FW") ~ "DF",
      lu_change == "FF" ~ "StableF",
      lu_change %in% c("CC", "GG", "OO", "SS", "WW") ~ "StableNF",
      TRUE ~ "StableNF"
    ),
    redd_FRL = case_when(
      !(lu_change_year %in% 2017:2021) & redd == "AF"      ~ "StableF",
      !(lu_change_year %in% 2017:2021) & redd == "DF"       ~ "StableNF",
      #!(lu_change_year %in% 2017:2021) & redd == "StableNF" ~ "StableNF",
      TRUE ~ redd
    )
  )

table(rev_all$operator) 

rev_valid_notmatchingFRL <- rev_all %>% 
  filter(id %in% valid_new_0_FRL$id) %>%
  distinct(id, .keep_all = TRUE)
  
rev_valid_DF <- rev_all %>%
  filter(id %in% valid_new_DF$id) %>%
  distinct(id, .keep_all = TRUE)

rev_valid_AF <- rev_all %>%
  filter(id %in% valid_new_AF$id) %>%
  distinct(id, .keep_all = TRUE)

rev_AD_DF <- rev_all %>%
  filter(id %in% DFnotinSBAE$id) %>%
  distinct(id, .keep_all = TRUE)

rev_AD_AF <- rev_all %>%
  filter(id %in% AFnotinSBAE$id) %>%
  distinct(id, .keep_all = TRUE)

rev_AD_SF <- rev_all %>%
  filter(id %in% SFtreelossSBAE$id) %>%
  distinct(id, .keep_all = TRUE)

# Group Validation exercises
rev_valid <- bind_rows(rev_valid_notmatchingFRL, rev_valid_AF, rev_valid_DF)

## Error based on new validation
rev_valid2 <- ce_valid %>%
  filter(!(id %in% rev_valid$id)) %>%
  bind_rows(rev_valid)

error_rev_valid_vs_AD <- ce_AD %>% 
  filter(id %in% rev_valid2$id) %>%
  left_join(rev_valid2, by = "id") %>%
  group_by(redd_FRL_AD, redd_FRL) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = redd_FRL, values_from = count)

error_rev_valid_vs_AD

acc_overall <- (4 + 248 + 66) / 425 * 100
acc_overall

acc_user_AF <- 4 / (4+7+5) * 100
acc_user_AF

acc_producer_AF <- 4 / (4+16+5) * 100
acc_producer_AF

## For reference
error_initial

(2 + 254 + 62) / 425


## Check AF alone --------------------------------------------------------------

ce_valid_AF <- ce_valid %>% filter(id %in% rev_valid_AF$id)

names(ce_valid_AF) <- names(ce_valid_AF) %>% 
  paste0(., "_valid") %>%
  str_replace("id_valid", "id")

error_rev_valid_vs_AD_AF <- ce_AD %>% 
  filter(id %in% valid_new_AF$id) %>%
  left_join(filter(rev_valid, id %in% valid_new_AF$id), by = "id") %>%
  group_by(redd_FRL_AD, redd_FRL) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = redd_FRL, values_from = count)

error_rev_valid_vs_AD_AF

## Many points with change in 2000
table(ce_AD$lu_change_year_AD)

test <- ce_results %>% filter(land_use_subcategory_year_of_change_label == 2000) 

table(test$plot_file)

test_id <- test %>% filter(!(id %in% rev_valid_AF$id)) %>% pull(id)

## Assigning plots with change in 2000 to be revisited 
ce_grid %>% filter(id %in% test_id) %>% write_csv(file.path(path_res, "AD-revision-change2000_notinvalid.csv"))


## Check validation revisited against validation

error_rev_valid_vs_valid <- rev_valid %>%
  left_join(ce_valid2, by = "id") %>%
  group_by(redd_FRL_valid, redd_FRL) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = redd_FRL, values_from = count)

  
  
##
## Checks in AD ################################################################
##

rev_AD <- bind_rows(rev_AD_AF, rev_AD_SF, rev_AD_DF)


## Calculate errors for AD revision --------------------------------------------

error_rev_AD_vs_AD <- rev_AD %>%
  left_join(ce_AD, by = "id") %>%
  group_by(redd_FRL_AD, redd_FRL) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = redd_FRL, values_from = count)

error_rev_AD_vs_AD

(4 + 20 + 3 + 2) / nrow(rev_AD)

rev_AD %>%
  filter(id %in% rev_AD_DF$id) %>%
  left_join(ce_AD, by = "id") %>%
  group_by(redd_FRL_AD, redd_FRL) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = redd_FRL, values_from = count)

rev_AD %>%
  filter(id %in% rev_AD_AF$id) %>%
  left_join(ce_AD, by = "id") %>%
  group_by(redd_FRL_AD, redd_FRL) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = redd_FRL, values_from = count)

rev_AD %>%
  filter(id %in% rev_AD_SF$id) %>%
  left_join(ce_AD, by = "id") %>%
  group_by(redd_FRL_AD, redd_FRL) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = redd_FRL, values_from = count)


##
## Merge Revision with AD ######################################################
##

print(ce_AD_init)
print(rev_all)

rev_all2 <- bind_rows(rev_AD, rev_valid)

test <- rev_all2 %>%
  group_by(id) %>%
  summarise(count = n())

table(test$count)



ce_AD_rev <- ce_AD_init %>% 
  filter(!(id %in% rev_all$id))



