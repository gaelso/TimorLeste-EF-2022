
library(tidyverse)

path_data <- "data/validation-revision"
dir.create(path_data, showWarnings = FALSE)

path_AD <- file.path(path_data, "AD recalculation")
dir.create(path_AD, showWarnings = F)

ce_AD_corr     <- read_csv(file.path(path_AD, "Lorena_CE_LUcorr.csv"))
ce_AD_rev_corr <- read_csv(file.path(path_AD, "AD_revision_LUcorr.csv"))

## FRL land use codes
lu_code <- tibble(
  lu_name = sort(unique(c(ce_AD_rev_corr$lu_cat, ce_AD_rev_corr$lu_cat_init))) %>% str_to_sentence(),
  lu_id   = c("FC", "C", "FDL", "FP", "G", "MF", "FMH", "FML", "FM", "O", "OWL", "S", "SH", "W"),
  lu_no   = c(   5,  11,     3,    7,   8,    6,     1,     2,    4,  14,    10,  12,    9,  13),
  lu_f    = fct_reorder(lu_id, lu_no)
) %>%
  select(lu_name, lu_f)

summary(lu_code$lu_f)

AD_rev_lu <- ce_AD_rev_corr %>%
  mutate(
    lu_cat = str_to_sentence(lu_cat),
    lu_cat_init = str_to_sentence(lu_cat_init)
    ) %>%
  left_join(lu_code, by = c("lu_cat" = "lu_name")) %>%
  rename(lu_code = lu_f) %>%
  left_join(lu_code, by = c("lu_cat_init" = "lu_name")) %>%
  rename(lu_code_init = lu_f)

AD_rev_nochange <- AD_rev_lu %>% 
  filter(is.na(lu_change_year)) %>%
  mutate(
    lu2022 = lu_code,
    lu2021 = lu_code,
    lu2020 = lu_code,
    lu2019 = lu_code,
    lu2018 = lu_code,
    lu2017 = lu_code,
    lu2016 = lu_code
    )

AD_rev_annual <- AD_rev_lu %>% 
  filter(!is.na(lu_change_year)) %>%
  mutate(
    lu2022 = lu_code,
    lu2021 = if_else(lu_change_year == 2022, lu_code_init, lu2022),
    lu2020 = if_else(lu_change_year == 2021, lu_code_init, lu2021),
    lu2019 = if_else(lu_change_year == 2020, lu_code_init, lu2020),
    lu2018 = if_else(lu_change_year == 2019, lu_code_init, lu2020),
    lu2017 = if_else(lu_change_year == 2018, lu_code_init, lu2020),
    lu2016 = if_else(lu_change_year == 2017, lu_code_init, lu2020),
  ) %>%
  bind_rows(AD_rev_nochange)


table(AD_rev_annual$lu_cat, AD_rev_annual$lu_code, useNA = 'ifany')
table(AD_rev_annual$lu_change_year)

luc21 <- AD_rev_annual %>%
  group_by(lu2020, lu2021) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = lu2021, values_from = count, values_fill = 0) %>%
  select(luc2021 = lu2020, FMH, FML, FDL, FM, FC, MF, FP, G, SH, OWL, C, S, W, O)
luc21

