
## +++ +++
## NFI design optimization scripts for Timor Leste
## +++ +++


## Plot optimization based on Practools::strAlloc().
## Input variables are:
## - n.tot  fixed total sample size
## - Nh	    vector of population stratum sizes (N_h) or pop stratum proportions (W_h)
## - Sh	    stratum unit standard deviations (S_h), required unless alloc = "prop"
## - cost	  total variable cost
## - ch	    vector of costs per unit in stratum h (c_h)
## - V0	    fixed variance target for estimated mean
## - CV0	  fixed CV target for estimated mean
## - ybarU  population mean of y (\bar{y}_U)
## - alloc  type of allocation; must be one of "prop", "neyman", "totcost", "totvar"



## + Function parameters ----

## + + Initial plot design ----

design_elements <- tibble(
  subplot_radius = 17.84,
  subplot_nb     = 4
  ) %>%
  mutate(
    subplot_area   = round(pi * subplot_radius^2 /100^2, 3),
    plot_area      = subplot_area * subplot_nb
    )
design_elements


## + + Parameters based on biomass data ----

## AGB data from Avitabile 2016 biomass map

## AGB map resolution as reference area in ha
pix_area <- terra::res(rs_agb)[1]^2 / 100^2

## Because variance conversion based on plot size reaches asymptot
pix_area <- if_else(pix_area > 1, 1, pix_area)


param_ABG <- tibble(
  Nh   = area_country / design_elements$plot_area,
  AGB  = agb_tot$agb_mean,
  Sh   = agb_tot$agb_sd,
  CV   = Sh / AGB * 100
  ) %>%
  mutate(
    AGB_plot = AGB * design_elements$plot_area,
    Sh_plot  = Sh * (design_elements$plot_area / pix_area)^0.5,
    CV_plot  = CV * (pix_area / design_elements$plot_area)^0.5,
    CV_plot2 = Sh_plot / AGB_plot,
    CV_plot3 = sqrt(CV^2 * (pix_area / design_elements$plot_area)^0.5), ## Lynch 2017 https://academic.oup.com/forestry/article/90/2/211/2605853
    Sh_plot3 = sqrt(Sh^2 * (pix_area / design_elements$plot_area)^0.5)
    )
param_ABG


## + + Cost calculation ----

## Cost is calculated in time required for tree measurements with calc_time().
## Input parameters:
## - 



#time to drive from plot to plot
c_1=c(10)#km/h average driving speed
#distance between subplots
dist_sub=60#m
#time to walk from plot to plot
v_1=c(2.5) #km/h average walking speed
#time to measure plot proportional to area and plot biomass
rho_1=rep(0.0035,no_strata)#h/m^2
#time to delimitate plot proportional to perimeter and weighted with plot biomass
nu_1=rep(0.0014,no_strata)#h/m
#time to extract permissions to measure from focus groups (per plot). Smaller in deep forest.
tp_1<-c(5)


## + Optimization ----


strAlloc_1A<- strAlloc(
  Nh = alloc_param$Nh, 
  Sh = alloc_param$Sh_plot, 
  CV0 = 0.1/qt(.95,df=Inf), 
  ch = T_1, 
  ybarU=alloc_param$AGB_plot,
  alloc = "totvar"
  )#ybarU is population mean
strAlloc_1A
sum(strAlloc_1A[["nh"]])


## INITIALIZATION PARAMETERS 
## simple Zeide's (1980) approach
pix_area = 1     #ha. of pixel in Avitabile's biomass map
#subplot_rad= 12.62# radius in m. of subplot 
subplot_rad=15
no_subplots=4   # no. subplots in previous inventories
#no_subplots=4
subplot_area= pi*subplot_rad^2/10000 # ha. of subplot (to be used if calculations at subplot level)
plot_area= subplot_area * no_subplots #ha. of cluster (to be used if calculations at cluster level)
plot_area
#no.strata
no_strata=1

## Liberia 1 strata
##Calculate rates for cost-related times
#time to drive from plot to plot
c_1=c(10)#km/h average driving speed
#distance between subplots
dist_sub=60#m
#time to walk from plot to plot
v_1=c(2.5) #km/h average walking speed
#time to measure plot proportional to area and plot biomass
rho_1=rep(0.0035,no_strata)#h/m^2
#time to delimitate plot proportional to perimeter and weighted with plot biomass
nu_1=rep(0.0014,no_strata)#h/m
#time to extract permissions to measure from focus groups (per plot). Smaller in deep forest.
tp_1<-c(5)