
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
    location_x,
    location_y,
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

# table(ce_AD$lu_sec_change_year_AD)
# ce_AD_2changes <- ce_AD %>% filter(lu_sec_change_year_AD %in% 2017:2021)
# 
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
    plot_file,
    operator,
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



##
## NOT USED, splitting the data to check detailed changes ######################
##

# rev_valid_notmatchingFRL <- rev_all %>% 
#   filter(id %in% valid_new_0_FRL$id) %>%
#   distinct(id, .keep_all = TRUE)
#   
# rev_valid_DF <- rev_all %>%
#   filter(id %in% valid_new_DF$id) %>%
#   distinct(id, .keep_all = TRUE)
# 
# rev_valid_AF <- rev_all %>%
#   filter(id %in% valid_new_AF$id) %>%
#   distinct(id, .keep_all = TRUE)
# 
# rev_AD_DF <- rev_all %>%
#   filter(id %in% DFnotinSBAE$id) %>%
#   distinct(id, .keep_all = TRUE)
# 
# rev_AD_AF <- rev_all %>%
#   filter(id %in% AFnotinSBAE$id) %>%
#   distinct(id, .keep_all = TRUE)
# 
# rev_AD_SF <- rev_all %>%
#   filter(id %in% SFtreelossSBAE$id) %>%
#   distinct(id, .keep_all = TRUE)
# 
# # Group Validation exercises
# rev_valid <- bind_rows(rev_valid_notmatchingFRL, rev_valid_AF, rev_valid_DF)
# 
# 
# 
# ## Error based on new validation -----------------------------------------------
# rev_valid2 <- ce_valid %>%
#   filter(!(id %in% rev_valid$id)) %>%
#   bind_rows(rev_valid)
# 
# error_rev_valid_vs_AD <- ce_AD %>% 
#   filter(id %in% rev_valid2$id) %>%
#   left_join(rev_valid2, by = "id") %>%
#   group_by(redd_FRL_AD, redd_FRL) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   pivot_wider(names_from = redd_FRL, values_from = count)
# 
# error_rev_valid_vs_AD
# 
# acc_overall <- (4 + 248 + 66) / 425 * 100
# acc_overall
# 
# acc_user_AF <- 4 / (4+7+5) * 100
# acc_user_AF
# 
# acc_producer_AF <- 4 / (4+16+5) * 100
# acc_producer_AF
# 
# ## For reference
# error_initial
# 
# (2 + 254 + 62) / 425
# 
# 
# ## Error based on new valid vs initial validation -------------------------------
# 
# error_rev_valid_vs_valid <- rev_valid %>%
#   left_join(ce_valid2, by = "id") %>%
#   group_by(redd_FRL_valid, redd_FRL) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   pivot_wider(names_from = redd_FRL, values_from = count)
# 
# error_rev_valid_vs_valid


  
##
## Checks in AD ################################################################
##

# rev_AD <- bind_rows(rev_AD_AF, rev_AD_SF, rev_AD_DF)


## Calculate errors for AD revision --------------------------------------------

# error_rev_AD_vs_AD <- rev_AD %>%
#   left_join(ce_AD, by = "id") %>%
#   group_by(redd_FRL_AD, redd_FRL) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   pivot_wider(names_from = redd_FRL, values_from = count)
# 
# error_rev_AD_vs_AD
# 
# (4 + 20 + 3 + 2) / nrow(rev_AD)
# 
# rev_AD %>%
#   filter(id %in% rev_AD_DF$id) %>%
#   left_join(ce_AD, by = "id") %>%
#   group_by(redd_FRL_AD, redd_FRL) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   pivot_wider(names_from = redd_FRL, values_from = count)
# 
# rev_AD %>%
#   filter(id %in% rev_AD_AF$id) %>%
#   left_join(ce_AD, by = "id") %>%
#   group_by(redd_FRL_AD, redd_FRL) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   pivot_wider(names_from = redd_FRL, values_from = count)
# 
# rev_AD %>%
#   filter(id %in% rev_AD_SF$id) %>%
#   left_join(ce_AD, by = "id") %>%
#   group_by(redd_FRL_AD, redd_FRL) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   pivot_wider(names_from = redd_FRL, values_from = count)



##
## Prepare unique values of the revision exercise ##############################
##

# print(ce_AD_init)
# print(rev_all)
# 
# rev_all2 <- bind_rows(rev_AD, rev_valid)
# 
# test <- rev_all2 %>%
#   group_by(id) %>%
#   summarise(count = n())
# 
# table(test$count)
# 
# test_id <- test %>% filter(count > 1) %>% pull(id)
# 
# test2 <- rev_all_init %>% 
#   filter(id %in% test_id) %>%
#   distinct(id, operator, land_use_category, .keep_all = T)
# 
# write_csv(test2, file.path(path_rev, "duplicates in revision.csv"))
# 
# 
# rev_all3 <- rev_AD %>% 
#   filter(!(id %in% test_id)) %>%
#   bind_rows(rev_valid) %>%
#   distinct(id, .keep_all = TRUE) %>%
#   filter(!(id %in% c(217, 4497))) ## Celeste file for validation is missing 2 points
# 
# length(unique(rev_all3$id))



##
## Removing duplicates from group exercise and duplicates ######################
##

length(unique(rev_all$id))

## Removing duplicates from backups 
rev_all2 <- rev_all %>%
  distinct(id, plot_file, operator, .keep_all = TRUE) %>%
  arrange(id)

## Removing duplicates from group exercise  
rev_1 <- rev_all2 %>% 
  filter(plot_file == "validation-revision-ce-points-June2023-NOTmatching_FRL.csv") %>% 
  distinct(id, .keep_all = TRUE)

## Remaining duplicates require manual editing (points cross-checked to select)
test <- rev_all2 %>% 
  filter(!(id %in% rev_1$id)) %>%
  summarise(count = n(), .by = id)
table(test$count)

test_id <- test %>% filter(count > 1) %>% pull(id)

rev_2 <- rev_all2 %>%
  filter(id %in% test_id) %>%
  filter(!(id %in% c(217, 4497, 1503) & operator == "Celeste")) %>%
  filter(!(id == 1453 & operator == "Jose Oqui"))

## Getting unique records  
rev_all3 <- rev_all2 %>% 
  filter(!(id %in% c(rev_1$id, rev_2$id))) %>%
  bind_rows(rev_1, rev_2)
  
length(unique(rev_all3$id)) == nrow(rev_all3)



##
## Validation all revised plot vs original #####################################
##

validation_all <- ce_valid %>% 
  filter(!(id %in% rev_all3$id)) %>% 
  bind_rows(rev_all3) %>%
  select(-plot_file, -operator, -starts_with("lu_sec"))

length(unique(validation_all$id)) == nrow(validation_all)

error_all <- validation_all %>%
  left_join(ce_AD, by = "id") %>%
  group_by(redd_FRL_AD, redd_FRL) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = redd_FRL, values_from = count) %>%
  select(redd_FRL_AD, AF, DF, StableF, StableNF)

error_all

acc_overall <- (8 + 2 + 266 + 69) / 514 * 100
acc_overall

acc_user_AF <- 8 / (8 + 16 + 10) * 100
acc_user_AF

acc_producer_AF <- 8 / (8 + 20 + 5) * 100
acc_producer_AF

## For reference
error_initial



##
## Additional corrections ######################################################
##

list.files(file.path(path_data, "Validation AD revision results"), full.names = TRUE)

## Correction of change year set to 2000
error2000_1 <- read_csv("data/validation-revision/Validation AD revision results/Celeste_collectedData_earthtimor_leste_2022_frel_v11_copy_on_220623_170123_CSV.csv")
error2000_2 <- read_csv("data/validation-revision/Validation AD revision results/Elvinonada_collectedData_earthtimor_leste_2022_frel_v11_copy_on_220623_171022_CSV.csv")

## Need to remove id 2965 from Celeste file
error2000_1$id
error2000_2$id

error2000 <- error2000_1 %>%
  filter(id != 2965) %>%
  bind_rows(error2000_2)

## 4 plots had no land use change recorded
table(ce_results$land_use_subcategory, useNA = "ifany")
table(ce_AD_init$lu_change, useNA = "ifany")
table(ce_AD_init$lu_cat, useNA = "ifany")

error_missing_luchange <- read_csv( "data/validation-revision/Validation AD revision results/Elvinonada_collectedData_earthtimor_leste_2022_frel_v11_copy_on_230623_182538_CSV.csv")

## Correction grouped and added to original data
error_correction <- error2000 %>%
  bind_rows(error_missing_luchange) %>%
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


##
## Merge Revision with AD ######################################################
##

validation_all %>% pull(id) %>% sort()
error_correction %>% pull(id) %>% sort()

revision_all <- validation_all %>%
  filter(!(id %in% error_correction$id)) %>%
  bind_rows(error_correction) 

ce_AD_init %>% filter(lu_change_year == 2022)

ce_AD_rev <- ce_AD_init %>%
  select(-starts_with("lu_sec")) %>% 
  filter(!(id %in% revision_all$id)) %>%
  bind_rows(revision_all) %>%
  mutate(lu_change_year = if_else(lu_change_year %in% 2017:2022, lu_change_year, NA_integer_)) 

## Compare with inital ce_AD
ce_AD_init <- ce_AD_init %>% 
  mutate(lu_change_year = if_else(lu_change_year %in% 2017:2022, lu_change_year, NA_integer_)) %>%
  select(-starts_with("lu_sec"))

table(ce_AD_init$redd_FRL)
table(ce_AD_rev$redd_FRL)

table(ce_AD_init$lu_change_year, ce_AD_init$redd_FRL, useNA= "ifany")
table(ce_AD_rev$lu_change_year, ce_AD_rev$redd_FRL, useNA= "ifany")



##
## Correction of NAs in the land use columns ###################################
##

ce_AD_corr <- ce_AD_init %>%
  mutate(
    lu_cat = case_when(
      str_sub(lu_change, 2, 2) == "C" ~ "Cropland",
      str_sub(lu_change, 2, 2) == "S" ~ "Settlements",
      str_sub(lu_change, 2, 2) == "W" ~ "Wetland",
      str_sub(lu_change, 2, 2) == "O" ~ "Other land",
      is.na(lu_cat) & str_sub(lu_change, 2, 2) == "G" ~ "grassland", ## Assume grassland when info not available
      TRUE ~ lu_cat
    )
  )

table(ce_AD_corr$lu_cat, useNA = "ifany")

ce_AD_rev_corr <- ce_AD_rev %>%
  mutate(
    lu_cat = case_when(
      str_sub(lu_change, 2, 2) == "C" ~ "Cropland",
      str_sub(lu_change, 2, 2) == "S" ~ "Settlements",
      str_sub(lu_change, 2, 2) == "W" ~ "Wetland",
      str_sub(lu_change, 2, 2) == "O" ~ "Other land",
      is.na(lu_cat) & str_sub(lu_change, 2, 2) == "G" ~ "grassland",
      TRUE ~ lu_cat
    ),
    lu_cat_init = case_when(
      is.na(lu_change_year)           ~ lu_cat,
      str_sub(lu_change, 1, 1) == "C" ~ "Cropland",
      str_sub(lu_change, 1, 1) == "S" ~ "Settlements",
      str_sub(lu_change, 1, 1) == "W" ~ "Wetland",
      str_sub(lu_change, 1, 1) == "O" ~ "Other land",
      is.na(lu_cat_init) & str_sub(lu_change, 1, 1) == "G" ~ "grassland",
      TRUE ~ lu_cat_init
    )
  )

table(ce_AD_rev_corr$lu_cat, useNA = "ifany")
table(ce_AD_rev_corr$lu_cat_init, useNA = "ifany")

ce_AD_rev_corr %>% filter(is.na(lu_cat_init))


path_AD <- file.path(path_data, "AD recalculation")
dir.create(path_AD, showWarnings = F)

write_csv(ce_AD_corr, file.path(path_AD, "Lorena_CE_LUcorr.csv"))
write_csv(ce_AD_rev_corr, file.path(path_AD, "AD_revision_LUcorr.csv"))






