
## Training on National Forest Inventory: Overview and data analysis for carbon 
## 09-13/05/2022, Dili, Timor Leste
## Gael Sola (FAO)


## Steps:
## - Calculate tree AGB
## - Aggregate to subplot
## - Calculate tree BGB
## - Aggregate to plot
## - Aggregate to forest type


## --- Setup ----------------------------------------------------------------

## Load library, if not installed, install it in the Package tab (bottom right panel)
library(tidyverse)

## Plot theme
theme_set(theme_bw())

## Load tables
plot_init <- read_csv("data/plot.csv")

tree_init <- read_csv("data/tree.csv")

wd_init <- read_csv("data/gwd.csv")

## First check
table(plot_init$lc)

summary(tree_init$tree_dbh)

summary(tree_init$tree_height_top)



## Data processing ----------------------------------------------------------

## Select only wood densities form Asia 
table(wd_init$Region)

wd <- wd_init %>%
  select(species_scientific_name = Binomial, region = Region, wd = `Wood density (g/cm^3), oven dry mass/fresh volume`) %>%
  filter(region %in% c("South-East Asia", "South-East Asia (tropical)")) %>%
  group_by(species_scientific_name) %>%
  summarise(tree_wd = mean(wd))


## Join the info from plot table (lc, forest_type, etc.)
## Join the wood densities values from wd table 
tree <- tree_init %>%
  left_join(plot_init, by = "plot_id") %>%
  left_join(wd, by = "species_scientific_name")
tree

## Recalculate DBH and basal area
tree_ba <- tree %>% 
  mutate(
    tree_dbh_cross = round(tree_girth/pi, 2),
    tree_ba_cross = round((tree_dbh_cross/200)^2 * pi, 2)
    )

ggplot(tree_ba) +
  geom_point(aes(x = tree_dbh, y = tree_ba))
         
ggplot(tree_ba) +
  geom_point(aes(x = tree_dbh, y = tree_dbh_cross))

ggplot(tree_ba) +
  geom_point(aes(x = tree_ba, y = tree_ba_cross))

ggplot(tree_ba) +
  geom_point(aes(x = tree_dbh, y = tree_height_top, color = plot_id, shape = site)) +
  scale_shape_manual(values = c(1, 3)) +
  facet_wrap(~site)

ggplot(tree_ba) +
  geom_line(aes(x = tree_dbh, y = tree_height_model, color = plot_id, linetype = site)) +
  scale_shape_manual(values = c(1, 3))


ggplot(tree_ba) +
  geom_point(aes(x = tree_dbh, y = tree_height_model, color = plot_id), shape = 1) +
  geom_point(aes(x = tree_dbh, y = tree_height_top, color = plot_id), shape = 2) +
  geom_segment(aes(x = tree_dbh, xend = tree_dbh, y = tree_height_top, yend = tree_height_model)) +
  facet_wrap(~site)

ggplot(tree_ba) + 
  geom_col(aes(x = species_scientific_name, y = tree_wd)) +
  coord_flip()

## Correct values
tree_ba_corr <- tree_ba %>%
  mutate(tree_ba = tree_ba_cross) %>%
  select(-tree_dbh_cross, -tree_ba_cross)

## Add wood density
tree_wd <- tree_ba_corr %>%
  left_join()


## Calculate tree AGB
tree_agb <- tree_ba_corr %>%
  mutate(
    agb_tree = case_when(                       ## <- case_when() replaces a lot of embedded ifelse() statements
      lc_class == "EF"  ~ 0.3112 * dbh^2.2331,
      lc_class == "DD"  ~ 0.2137 * dbh^2.2575,
      lc_class == "MDF" ~ 0.523081 * dbh^2,
      lc_class == "CF"  ~ 0.1277 * dbh^2.3944,
      lc_class == "MCB" ~ 0.1277 * dbh^2.3944,
      TRUE ~ NA_real_   # <- assigns NA to any case not covered by the conditions above
    )
  ) 


## With tidyverse all these operations can be condensed to one sequence of actions:
tree <- tree_init %>%
  filter(class_code == 0) %>%
  left_join(subplot) %>%
  filter(lc_class %in% c("EF", "MDF", "DD", "CF", "MCB")) %>%
  mutate(
    ba_tree = round((dbh/200)^2 * pi, 2),
    agb_tree = case_when(                       ## <- case_when() replaces a lot of embedded ifelse() statements
      lc_class == "EF"  ~ 0.3112 * dbh^2.2331,
      lc_class == "DD"  ~ 0.2137 * dbh^2.2575,
      lc_class == "MDF" ~ 0.523081 * dbh^2,
      lc_class == "CF"  ~ 0.1277 * dbh^2.3944,
      lc_class == "MCB" ~ 0.1277 * dbh^2.3944,
      TRUE ~ NA_real_   # <- assigns NA to any case not covered by the conditions above
    )
  )
  

## CHECKS
table(tree$lc_class, useNA = 'always')
table(tree$class_code, useNA = 'always')

## VISUAL CHECKS
tree %>% 
  ggplot(aes(x = dbh, y = agb_tree, color = lc_class, shape = lc_class)) +
  geom_point() 

tree %>% 
  ggplot(aes(x = dbh, y = agb_tree, color = lc_class, shape = lc_class)) +
  geom_point() +
  theme_bw() +
  labs(
    x = "Tree diameter at breast height (cm)",
    y = "Tree abovegrond biomass (kg)",
    color = "Land cover class",
    shape = "Land cover class"
  )



tree %>%
  ggplot(aes(x = dbh, y = lc_class)) +
  geom_boxplot()


tree %>%
  ggplot(aes(x = dbh, y = lc_class, color = subplot_id)) +
  geom_boxplot()



## --- tree to subplot AGB --------------------------------------------------

subplot_agb <- tree %>%
  group_by(subplot_no) %>%
  summarise(
    n_trees =  scale_factor,
    ba_sp = sum(ba_tree * scale_factor),
    agb_sp = sum(agb_tree * scale_factor * 0.001), ## <- 0.001 to convert kg to ton
  ) %>%
  left_join(subplot) %>%
  mutate(
    bgb_sp = case_when(
      lc_class == "CF" & agb_sp <   50 ~ agb_sp * 0.46,
      lc_class == "CF" & agb_sp <= 150 ~ agb_sp * 0.32,      
      lc_class == "CF" & agb_sp >  150 ~ agb_sp * 0.23,
      lc_class != "CF" & agb_sp <  125 ~ agb_sp * 0.20,
      lc_class != "CF" & agb_sp >= 125 ~ agb_sp * 0.24,
      TRUE ~ 0 ## <- no BGB for other cases, i.e. dead trees in the complete analysis
     ) ,
    carbon_sp = (agb_sp + bgb_sp) * 0.47
  )

## Checks 
subplot_agb %>%
  ggplot(aes(x = agb_sp, y = lc_class, color = lc_class)) +
  geom_boxplot()


subplot_agb %>%
  ggplot(aes(x = ba_sp, y = agb_sp, color = lc_class)) +
  geom_point()

subplot_agb %>%
  ggplot(aes(x = ba_sp, y = agb_sp, color = lc_class)) +
  geom_point(alpha = 0.6) +
  theme_bw() +
  labs(x = "Basal Area (m2/ha)", y = "Aboveground biomass (t/ha)", color = "Land cover")



## -- Aggregate subplots to plot --------------------------------------------

## plot_init contains lc_class_main, the main lc_class at plot level
## We need to filter only the subplots which have the same lc_class as lc_class_main
## 1. Join subplot_agb and plot_init
## 2. filter based on lc_class

plot_agb1 <- subplot_agb %>%
  left_join(plot_init) %>% 
  filter(lc_class == lc_class_main) 

## Then aggregate subplot to plot with mean values
plot_agb2 <- plot_agb1 %>%
  group_by(plot_id, lc_class_main, nb_subplot_lc_main) %>%
  summarise(
    n_trees  = mean(n_trees),
    ba       = mean(ba_sp),
    agb      = mean(agb_sp),
    bgb      = mean(bgb_sp),
    carbon   = mean(carbon_sp)
  )

## The same but in one sequence
plot_agb <- subplot_agb %>%
  left_join(plot_init) %>% 
  filter(lc_class == lc_class_main) %>%
  group_by(plot_id, lc_class_main, nb_subplot_lc_main) %>%
  summarise(
    n_trees  = mean(n_trees),
    ba       = mean(ba_sp),
    agb      = mean(agb_sp),
    bgb      = mean(bgb_sp),
    carbon   = mean(carbon_sp)
  )


## Check nb plot is consistent
plot_list <- subplot_agb %>% pull(plot_id) %>% unique() %>% sort()
length(plot_list)

plot_list2 <- plot_agb %>% pull(plot_id) %>% unique() %>% sort()
length(plot_list2)


## Checks
plot_agb %>%
  ggplot(aes(x = lc_class_main, y = agb, fill = lc_class_main)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(
    x = "Main land cover class",
    y = "Aboveground biomass (t/ha)"
  )

plot_agb %>% 
  ggplot(aes(x = ba, y = agb, color = lc_class_main, shape = lc_class_main)) +
  geom_point()


plot_agb %>%
  ggplot(aes(x = agb, y = bgb, color = lc_class_main)) +
  geom_point()


## --- Aggregate plots to forest type ---------------------------------------

ftype <- plot_agb %>%
  group_by(lc_class_main) %>%
  summarise(
    n_plot     = n(),
    agb_all    = round(mean(agb), 3),
    bgb_all    = round(mean(bgb), 3),
    carbon_tot = round(mean(carbon), 3),
    sd_carbon  = sd(carbon)
  ) %>%
  mutate(
    ci      = sd_carbon / sqrt(n_plot) * round(qt(0.975, n_plot-1), 2),
    ci_perc = round(ci / carbon_tot * 100, 0)
  )
ftype

ftype %>%
  ggplot(aes(x = lc_class_main, y = carbon_tot)) +
  geom_col(aes(fill = lc_class_main), col = "grey25") +
  geom_errorbar(aes(ymin = carbon_tot + ci, ymax = carbon_tot - ci, width = 0.5), col = "grey25") +
  geom_label(aes(y = if_else(is.na(sd_carbon), carbon_tot + 10, carbon_tot + ci + 10), label = n_plot))


