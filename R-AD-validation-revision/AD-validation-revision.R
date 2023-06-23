
library(tidyverse)
library(googledrive)

googledrive::drive_auth()

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
## Preparation of Validation revision survey ###################################
##

if (length(list.files(path_data)) == 0) {
  
  ## Check ggdrive folder
  googledrive::drive_ls(path = "TimorLeste/ActityData")
  
  ## Get googledrive ID for the inital validation results
  # googledrive::drive_find(pattern = "validation_allgroups_round2.csv", n_max = 10)
  ## ID is: 1b1o42L1TJW1zAJKluTTZVN_EIVNM-kQn
  
  ## Download validation results
  googledrive::drive_download(
    file = as_id("1b1o42L1TJW1zAJKluTTZVN_EIVNM-kQn"), 
    path = file.path(path_data, "validation_allgroups_round2.csv")
  )
  
  ## Prepare Folder target of CE grid points for AD
  gg_ce_grid <- googledrive::drive_ls(path = "TimorLeste_points_hex_2km_CE.csv_8_random/div_8")
  
  ## Download CE grid points for AD
  walk(1:nrow(gg_ce_grid), function(x){
    
    googledrive::drive_download(
      file = as_id(gg_ce_grid$id[x]),
      path = file.path(path_data2, gg_ce_grid$name[x])
    )
    
  })
  
} ## END IF



##
## Read validation and grid points and prepare clean validation points #########
##

list.files(path_data)

valid_points <- read_csv(file.path(path_data, "validation_allgroups_round2.csv"))

valid_id <- sort(valid_points$id)

ce_grid <- map_dfr(list.files(path_data2, pattern = ".csv", full.names = TRUE), read_csv) %>%
  arrange(id)

valid_new <- ce_grid %>% 
  filter(id %in% valid_id) %>%
  arrange(id)

write_csv(valid_new, file.path(path_res, "validation-revision-ce-points-June2023.csv"))

write_csv(ce_grid, file.path(path_res, "AD-ce-points-rebuilt-June2023.csv"))

##
## Read AD data cleaned from Lorena ############################################
##

if (!("Results_calculated_Lorena_surveyy_v10.csv" %in% list.files(path_data3))) {
  
  ## Search for latest files
  # googledrive::drive_find("Export from Collect Earth V.10", n_max = 10)
  ## Res ID: 1no-GYt7zMT0GNyhKtWbj0FfayqT6o39-
  
  file_id <- drive_ls(path = as_id("1no-GYt7zMT0GNyhKtWbj0FfayqT6o39-"), pattern = ".csv")
  
  googledrive::drive_download(
    file = as_id(file_id$id),
    path = file.path(path_data3, "Results_calculated_Lorena_surveyy_v10.csv")
  )
  
}

ce_results <- read_csv(file.path(path_data3, "Results_calculated_Lorena_surveyy_v10.csv"))

ce_results_val <- ce_results %>% 
  filter(id %in% valid_id) %>%
  arrange(id)

table(ce_results_val$land_use_category_label)
table(ce_results_val$land_use_subcategory)
table(ce_results_val$land_use_subdivision_label)
table(ce_results_val$land_use_initial_subdivision_label)
table(ce_results_val$land_use_subcategory_year_of_change)

write_csv(ce_results_val, file.path(path_data4, "AD results on validation points.csv"))



##
## Compare validation with AD ##################################################
##

ce_results_val <- read_csv(file.path(path_data4, "AD results on validation points.csv"))

ce_results_val2 <- ce_results_val %>%
  select(
    id, lu_change = land_use_subcategory, lu_cat = land_use_subdivision_label, 
    lu_cat_init = land_use_initial_subdivision_label, lu_change_year = land_use_subcategory_year_of_change
    )

names(ce_results_val2) <- names(ce_results_val2) %>% 
  paste0(., "_AD") %>%
  str_replace("id_AD", "id")

names(ce_results_val2)


ce_valid <- valid_points %>%
  select(
    id, lu_change = land_use_subcategory, lu_cat = land_use_subdivision_label, 
    lu_cat_init = land_use_initial_subdivision_label, lu_change_year = land_use_subcategory_year_of_change
    )
 
names(ce_valid) <- names(ce_valid) %>%
  paste0(., "_valid") %>%
  str_replace("id_valid", "id")


ce_compa <- ce_results_val2 %>%
  left_join(ce_valid, by = "id") %>%
  mutate(
    redd_AD = case_when(
      lu_change_AD %in% c("CF", "GF", "OF", "SF", "WF") ~ "AF",
      lu_change_AD %in% c("FC", "FG", "FO", "FS", "FW") ~ "DF",
      lu_change_AD == "FF" ~ "Stable F",
      lu_change_AD %in% c("CC", "GG", "OO", "SS", "WW") ~ "Stable NF",
      TRUE ~ "Change in NF"
      ),
    redd_valid = case_when(
      lu_change_valid %in% c("CF", "GF", "OF", "SF", "WF") ~ "AF",
      lu_change_valid %in% c("FC", "FG", "FO", "FS", "FW") ~ "DF",
      lu_change_valid == "FF" ~ "Stable F",
      lu_change_valid %in% c("CC", "GG", "OO", "SS", "WW") ~ "Stable NF",
      TRUE ~ "Change in NF"
    ),
    redd_match = if_else(redd_AD == redd_valid, 1, 0)
  )

table(ce_compa$redd_AD)
table(ce_compa$redd_match)


ce_compa_1 <- ce_compa %>% filter(redd_match == 1)
ce_compa_0 <- ce_compa %>% filter(redd_match == 0)
ce_compa_0_FRL <- ce_compa %>% filter(redd_match == 0, lu_change_year_AD %in% c(2017:2021))
ce_compa_0_out <- ce_compa %>% filter(redd_match == 0, !(lu_change_year_AD %in% c(2017:2021)))

ce_compa_match <- bind_rows(ce_compa_1, ce_compa_0_out)

ce_compa_change_DF <- ce_compa_match %>% filter(redd_AD == "DF")
ce_compa_change_AF <- ce_compa_match %>% filter(redd_AD == "AF")
ce_compa_change_StableF <- ce_compa_match %>% filter(redd_AD == "Stable F")

write_csv(ce_compa, file.path(path_res, "comparison_AD_validation.csv"))




valid_new_1 <- valid_new %>% filter(id %in% ce_compa_1$id)
valid_new_0 <- valid_new %>% filter(id %in% ce_compa_0$id)
valid_new_0_FRL <- valid_new %>% filter(id %in% ce_compa_0_FRL$id)
valid_new_0_out <- valid_new %>% filter(id %in% ce_compa_0_out$id)

write_csv(valid_new_1, file.path(path_res,  "validation-revision-ce-points-June2023-matching.csv"))
write_csv(valid_new_0, file.path(path_res,  "validation-revision-ce-points-June2023-NOTmatching.csv"))
write_csv(valid_new_0_FRL, file.path(path_res,  "validation-revision-ce-points-June2023-NOTmatching_FRL.csv"))
write_csv(valid_new_0_out, file.path(path_res,  "validation-revision-ce-points-June2023-NOTmatching_outFRL.csv"))

valid_new_match <- bind_rows(valid_new_1, valid_new_0_out)

valid_new_DF <- valid_new_match %>% filter(id %in% ce_compa_change_DF$id)
valid_new_AF <- valid_new_match %>% filter(id %in% ce_compa_change_AF$id)

set.seed(10)
valid_new_AF_split <- split(valid_new_AF, sample(rep(1:4, times = c(28, 27, 28, 27))))

write_csv(valid_new_DF, file.path(path_res, "validation-revision-ce-points-June2023-matchingDF.csv"))

write_csv(valid_new_AF_split[[1]], file.path(path_res, "validation-revision-ce-points-June2023-matchingAF_1.csv"))
write_csv(valid_new_AF_split[[2]], file.path(path_res, "validation-revision-ce-points-June2023-matchingAF_2.csv"))
write_csv(valid_new_AF_split[[3]], file.path(path_res, "validation-revision-ce-points-June2023-matchingAF_3.csv"))
write_csv(valid_new_AF_split[[4]], file.path(path_res, "validation-revision-ce-points-June2023-matchingAF_4.csv"))

##
## ADD SBAE results ############################################################
##

esbae <- read_csv(file.path(path_data, "esbae_frel_timor.csv"))


esbae_AD <- esbae %>%
  mutate(
    esri_loss_year = case_when(
      esri_lc17 == 2 & esri_lc18 != 2 ~ 2018,
      esri_lc18 == 2 & esri_lc19 != 2 ~ 2019,
      esri_lc19 == 2 & esri_lc20 != 2 ~ 2020,
      esri_lc20 == 2 & esri_lc21 != 2 ~ 2021,
      TRUE ~ NA_integer_
    ),
    esri_gain_year = case_when(
      esri_lc17 != 2 & esri_lc18 == 2 ~ 2018,
      esri_lc18 != 2 & esri_lc19 == 2 ~ 2019,
      esri_lc19 != 2 & esri_lc20 == 2 ~ 2020,
      esri_lc20 != 2 & esri_lc21 == 2 ~ 2021,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(PLOTID, starts_with("bfast"), starts_with("ccdc"), esri_loss_year, esri_gain_year, gfc_gain, gfc_lossyear, tmf_defyear, tmf_degyear)

esbae_valid <- esbae_AD %>% 
  filter(PLOTID %in% valid_id) %>%
  select(PLOTID, starts_with("bfast"), starts_with("ccdc"), starts_with("gfc"), starts_with("tmf"), starts_with("esri"))


## harmonize CE AD
ce_AD <- ce_results %>%
  select(
    id, lu_change = land_use_subcategory, lu_cat = land_use_subdivision_label, 
    lu_cat_init = land_use_initial_subdivision_label, lu_change_year = land_use_subcategory_year_of_change
  ) %>%
  mutate(
    redd = case_when(
      lu_change %in% c("CF", "GF", "OF", "SF", "WF") ~ "AF",
      lu_change %in% c("FC", "FG", "FO", "FS", "FW") ~ "DF",
      lu_change == "FF" ~ "Stable F",
      lu_change %in% c("CC", "GG", "OO", "SS", "WW") ~ "Stable NF",
      TRUE ~ "Change in NF"
    ),
    redd_FRL = case_when(
      !(lu_change_year %in% 2016:2021) & redd == "AF" ~ "Stable F",
      !(lu_change_year %in% 2016:2021) & redd == "DF" ~ "Stable NF",
      !(lu_change_year %in% 2016:2021) & redd == "Change in NF" ~ "Stable NF",
      TRUE ~ redd
    )
  )

names(ce_AD) <- names(ce_AD) %>% 
  paste0(., "_AD") %>%
  str_replace("id_AD", "id")

ce_AD_sbae <- ce_AD %>%
  left_join(esbae_AD, by = c("id" = "PLOTID"))

write_csv(ce_AD_sbae, file.path(path_data4, "comparison_AD_esbae.csv"))

ce_AD_sbae_valid <- ce_AD_sbae %>% 
  mutate(
    to_check = case_when(
      redd_FRL_AD == "DF" & gfc_lossyear < 17                 ~ 1,
      redd_FRL_AD == "DF" & is.na(gfc_lossyear)               ~ 1,
      redd_FRL_AD == "AF" & !(esri_gain_year %in% 2017:2021)  ~ 2,
      redd_FRL_AD == "AF" & gfc_gain == 0                     ~ 3,
      redd_FRL_AD == "Stable F" & (gfc_lossyear %in% 17:21) ~ 4,
      gfc_lossyear %in% 17:21                                 ~ 10,
      TRUE ~ 0
    )
  )

table(ce_AD_sbae_valid$redd_FRL_AD)
table(ce_AD_sbae_valid$to_check)

set.seed(10)
AFnotinSBAE    <- ce_AD_sbae_valid %>% filter(to_check == 2) %>% slice_sample(n = 30) 
DFnotinSBAE    <- ce_AD_sbae_valid %>% filter(to_check == 1) 
SFtreelossSBAE <- ce_AD_sbae_valid %>% filter(to_check == 4)

ce_grid %>%
  filter(id %in% DFnotinSBAE$id) %>%
  write_csv(file.path(path_res, "AD-revision-DFnotinSBAE.csv"))

ce_grid %>%
  filter(id %in% AFnotinSBAE$id) %>%
  write_csv(file.path(path_res, "AD-revision-AFnotinSBAE.csv"))

ce_grid %>%
  filter(id %in% SFtreelossSBAE$id) %>%
  slice(1:20) %>%
  write_csv(file.path(path_res, "AD-revision-SFtreelossSBAE_1.csv"))

ce_grid %>%
  filter(id %in% SFtreelossSBAE$id) %>%
  slice(21:39) %>%
  write_csv(file.path(path_res, "AD-revision-SFtreelossSBAE_2.csv"))


## Check agreement between RS and AD
ce_AD_sbae_valid %>% 
  filter(redd_FRL_AD == "DF" & gfc_lossyear > 17) %>%
  pull(id)


## 


