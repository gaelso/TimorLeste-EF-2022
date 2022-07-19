##############################################################################
## Code for the optimal calculation and plotting of
## cluster plots in Liberia
## Time data taken from Picard in Cameroon
## The final calculation of weeks in practice exceeded the 21 weeks predicted
## by around two months(total around 28 weeks in the final Liberia NFI)
## (c) Javier G. P. Gamarra 2018
##############################################################################

#############################################################################
## FIND SAMPLE SIZE FOR LIBERIA ----------------------------------------------
#############################################################################

##
## GAEL: Here I have some concern that if area_ref is large (say 1km2 plot like Avitabile 2016)
##       and area_new is small (typical plot size < 0.5), scale_stdev() will create very small 
##       output and scale_CV a very large one. 
##

#Scaling functions between plot sizes, for std dev and CV
scale_stdev=function(a_ref,a_new,stdev_ref,expon){
  ##a_ref is area of reference (i.e., 1 ha pixel)
  ##a_new is the new area from which scaled stdev is wanted
  ##expon=0.43 in tropical countries
  ##WATCH Out!! STD DEV normalizes by a factor between size plots that is inverse from the one for CV!!
  newstd=stdev_ref*(a_new/a_ref)^expon
  return(newstd)
}
scale_CV=function(a_ref,a_new,CV_ref,expon){
  ##a_ref is area of reference (i.e., 1 ha pixel)
  ##a_new is the new area from which scaled CV is wanted
  ##expon=0.43 in tropical countries
  newstd=CV_ref*(a_ref/a_new)^expon
  return(newstd)
}


Cost_plot_hours<-function(n0,Nh,AGB,subplotsize,n_subplots,d_subplots,c=NULL,v=NULL,rho=NULL,nu=NULL,tp=NULL){
  ## calculation of costs (from Picard's Guide methodologique d'evaluation rapide des bois)
  #Now tw is the time to walk from plot to plot. It assumes n0=400 plots. Areas are converted
  #to km here
  #d_dubplots is the distance between subplots. #1000 in denominator transforms to km
  ## Walking assumes a cross where team has to go back to center subplot for every side of the cross
  tw<-if(!is.null(v)) d_subplots*(n_subplots-1)*2/(1000*v) else rep(0,length(Nh))
  #Now tc is the time to drive from plot to plot. It assumes n0=400 plots. Areas are converted
  #to km here
  tc<-if(!is.null(c)) sqrt(sum(Nh/100)/(n0*Nh/sum(Nh)))/c else rep(0,length(Nh))
  #time to measure plot proportional to area and plot biomass
  tm<-if(!is.null(rho)) rho*subplotsize*no_subplots*10000*AGB/max(AGB) else rep(0,length(AGB))
  #tm<-if(!is.null(rho)) rho*subplotsize*no_subplots*10000*AGB else rep(0,length(AGB))
  #time to delimitate plot proportional to perimeter and weighted with plot biomass
  td<-if(!is.null(nu)) 2*nu*pi*sqrt(subplotsize*10000/pi)*no_subplots*sqrt(AGB)/max(sqrt(AGB)) else rep(0,length(AGB))
  #time to request permission
  tp<-if(!is.null(tp)) tp else rep(0,length(Nh))
  ##Total cost per plot
  T<-tc+tw+tm+td+tp
  #  return(T)
}



## INITIALIZATION PARAMETERS 
## simple Zeide's (1980) approach

##
## GAEL: pix_area should be 100 as everything else seems to be in ha in the script
##

pix_area= 1     #ha. of pixel in Avitabile's biomass map



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


##
## GAEL: I commented out this part and added directly the total area to save time
##

# #READ SHAPEFILE
# library(rgdal)
# library(rgeos)
# library(raster)
# # File in FAO laptop
# #lr_border <- readOGR(dsn="C:/Users/garciaperezj/Desktop/Liberia/LiberiaNoMangroves/Liberia/Liberia_shp_witout_mangrove.shp")
# # File in Javier laptop
# #lr_border <- readOGR(dsn="C:/Users/javier/Desktop/Collect/Liberia_March2018/LiberiaNoMangroves/Liberia/Liberia_shp_witout_mangrove.shp")
# lr_border <- readOGR(dsn="data/javier/LiberiaNoMangroves/Liberia/Liberia_shp_witout_mangrove.shp")
# ## We need units in m. Project with EPSG for Liberia obtained from https://epsg.io/
# ## Google "liberia utm zone epgs" and go to the first page in epsg.io. Result:32629
# lr_transformed <- spTransform(lr_border, CRS("+init=epsg:32629"))
# #area_ha<-gArea(lr_transformed)/10000#in ha
# area_ha<-sf::st_area(st_as_sf(lr_transformed))
# area_ha_mangrove<-10738.13
# proj4string(lr_transformed)
# area_ha
# lib_area<-as.numeric(sum(area_ha)) /100^2  + area_ha_mangrove
# plot(lr_border)
lib_area <- 9587189

##
## GAEL: Values of AGB_1A and Sh_1A below are from all pixels from Avitabile 2016 falling 
##       in Liberia right?
##

## Liberia 1 strata:Avitabile 
# Strata: Forest Management Concessions(100 x 100 m),# rest of country (100 x 100 m)
Nh_1A=c(lib_area)#has
AGB_1A=c(247.105485415004)    # AGB mean  
Sh_1A=c(150.651371052568)       # standard dev. (100 x 100 m)
CV_1A=Sh_1A/AGB_1A*100

#Scale CV and std dev. at cluster(plot) level. Assumes larger circle of 15 m radius
#AGB_2A_plotnew<-AGB_2A*plotnew_size

## GAEL: AGB_1A plot ton/ha * ha -> ton
AGB_1A_plot<-AGB_1A*plot_area

## GAEL: applying directly to CV in %, plot_area and pix_area should be both in ha
CV_1A_plot=scale_CV(pix_area,plot_area,CV_1A,0.5)   # CV (%) per ha in plot FMC
Sh_1A_plot=scale_stdev(pix_area,plot_area,Sh_1A,0.5)  # stdev per ha in plot FMC

## GAEL: Works fine with pix_area = 1 but gives weird results if pix_area = 100
CV_1A;CV_1A_plot;#comparing CV's
Sh_1A;Sh_1A_plot #comparing Sh's


## 
## GAEL: commented out this part as your indication, started with n_init = 356
##

# ############################################################################
# #METHOD 1: samplingbook: 
# ###NOT GOOD, COZ ONE HAS TO FIX THE TOTAL NUMBER OF PLOTS!!!##################
# ############################################################################
# ## #Find preliminary sample size. First input defines 10% error precision as 10% of BIOMASS
# ##ssmE<-sample.size.mean(0.1*mean(populationS1)/(S1/S2),sd(populationS1)/(S1/S2),length(populationS2),level=0.95)#Sample size needed: 135
# #2nd iteration modifying sample size df
# ##ssmE<-sample.size.mean(0.1*mean(populationS1)/(S1/S2),sd(populationS1)/(S1/S2),length(populationS2),level=0.95)#Sample size needed: 133
# ##ssmE$n#[1] 141
# library(samplingbook)
# ssize_1A<-NA
# for (i in 1:length(Nh_1A)){
#   kk<-sample.size.mean(e=(0.1*AGB_1A_plot[i]),S=Sh_1A_plot[i],N=Nh_1A[i]/plot_area,level=0.9)#e= half width of confidence interval
#   ssize_1A[i]<-kk$n
# }
# ssize_1A;#[1]  356
# n_init<-ssize_1A

n_init <- 356

T_1<-Cost_plot_hours(n0=n_init,Nh=Nh_1A,AGB=AGB_1A,subplotsize=subplot_area,n_subplots=no_subplots,d_subplots=dist_sub,c=c_1,v=v_1,rho=rho_1,nu=nu_1,tp=tp_1)
T_1

#Calculate total time for inventory
n_teams<-6
n_days_month<-20
n_hours_day<-8
n_days_week<-5
n_init*T_1#number of hours or whole inventory for one team
n_init*T_1/n_teams#number of hours or whole inventory for six team
n_init*T_1/(n_teams*n_hours_day)#number of days or whole inventory for six team
n_init*T_1/(n_teams*n_hours_day*n_days_month)#number of months or whole inventory for six team


#METHOD 4: PracTools 
###########################################################################################
library(PracTools)
#Liberia 1 strata: Neyman allocation. Fixed total plots
#Watch out!! results T
stud_t_90<-qt(.95,df=Inf)
strAlloc_1A<- strAlloc(Nh = Nh_1A/plot_area, Sh = Sh_1A_plot, CV0 = 0.1/stud_t_90, ch = T_1, ybarU=AGB_1A_plot,alloc = "totvar")#ybarU is population mean
strAlloc_1A;sum(strAlloc_1A[["nh"]])


#############################################################################
## For LOOP to determine total costs and number of samples
## for over number of subplots and radius per subplot
## in this example I only use samplesize4surveys
###########################################################################
grid_nsubplots<-seq(1,15,1)
grid_radius<-seq(10,35,1)
grid_cost<-expand.grid(x=grid_radius,y=grid_nsubplots) 
grid_nsamp<-expand.grid(x=grid_radius,y=grid_nsubplots)

for (i in grid_nsubplots){#no of subplots
  for (j in grid_radius){#radius
    subplot_area_2= pi*j^2/10000 # ha. of subplot (to be used if calculations at subplot level)
    plot_area_2= subplot_area_2 * i #ha. of cluster (to be used if calculations at cluster level)
    AGB_1A_plot_2<-AGB_1A*plot_area_2
    Sh_1A_plot_2=scale_stdev(pix_area,plot_area_2,Sh_1A,0.5)  # stdev per ha in plot FMC
    strAlloc_1A_2<- strAlloc(Nh = Nh_1A/plot_area_2, Sh = Sh_1A_plot_2, CV0 = 0.1/stud_t_90, ch = T_1, ybarU=AGB_1A_plot_2,alloc = "totvar")#ybarU is population mean
    n_init_2<-strAlloc_1A_2$nh
    T_1_2<-Cost_plot_hours(n0=n_init_2,Nh=Nh_1A,AGB=AGB_1A,subplotsize=subplot_area_2,n_subplots=i,d_subplots=dist_sub,c=c_1,v=v_1,rho=rho_1,nu=nu_1,tp=tp_1) 
    #tot_hours_1team<-n_init*T_1#number of hours or whole inventory for one team
    tot_weeks_6team<-n_init_2*T_1_2/(n_teams*n_hours_day*n_days_week)#number of months or whole inventory for six team
    id<-which(grid_cost$x==j & grid_cost$y==i)
    grid_cost$z[id]<-ceiling(tot_weeks_6team)
    grid_nsamp$z[id]<-ceiling(n_init_2)
  }
}


##PLOTTING GRAPH OF COSTS
###############################
library(lattice)
library(latticeExtra)
library(grid)
#color ramp
col.l <- colorRampPalette(c('red', 'orange', 'yellow', 'green', 'cyan', 'blue'))
colorplot<-
  levelplot(
    z ~ x*y, 
    grid_cost,
    col.regions=col.l,
    ylab = "No. subplots", xlab = "Subplot radius",
    #at=seq(from=pretty(min(grid_cost$z))[1],to=pretty(max(grid_cost$z))[2],length=80),   
    at=pretty(min(grid_cost$z):max(grid_cost$z),n=20),
    lattice.options=list(key=list(cex=4)),
    #panel = panel.2dsmoother,
    interpolate=TRUE,
    #colorkey=list(at=as.numeric(factor(c(seq(from=pretty(min(grid_cost$z))[1], to=pretty(max(grid_cost$z))[2], by=5)))),
    #              labels= as.character(seq(from=pretty(min(grid_cost$z))[1], to=pretty(max(grid_cost$z))[2], by=5)),
    #              height=1,width=2,col=(col.l)
    #              ,title="Weeks"
    #              ),
    colorkey=list(at=as.numeric(factor(pretty(min(grid_cost$z):max(grid_cost$z),n=20))),
                  labels= as.character(pretty(min(grid_cost$z):max(grid_cost$z),n=20)),
                  height=1,width=2,col=(col.l)
                  ,title="Weeks"
    ),
    main=list('No. weeks/no. plots',side=1,line=0.5)
  )

contourplot<- 
  contourplot(
    z ~ x*y, 
    grid_nsamp,
    #at=pretty(min(grid_nsamp$z):max(grid_nsamp$z),n=40), 
    at=c(50,100,200,300,500,1000,2000), 
    #panel=panel.2dsmoother,
    label.style=("flat"),
    lwd=1,
    #labels=(list(cex=1,labels=pretty(min(grid_nsamp$z):max(grid_nsamp$z),n=40)))
    labels=(list(cex=1.4,labels=(c("50", "100", "200", "300", "500", "1000","2000"))))
  ) 

(final.plot= (colorplot + latticeExtra::as.layer(contourplot)))
trellis.focus("panel", 1, 1, highlight=T) 
lpoints(15,5,pch=16)
ltext(14.5,5.4,cex=1.2,paste0("no. weeks =",round(grid_cost$z[which(grid_cost$x==15 & grid_cost$y==5)], 2)),pos=3)
ltext(14.5,5,cex=1.2,paste0("no. plots =",ceiling(grid_nsamp$z[which(grid_cost$x==15 & grid_cost$y==5)])),pos=3)
lpoints(18,5,pch=16)
ltext(18,5.4,cex=1.2,paste0("no. weeks =",round(grid_cost$z[which(grid_cost$x==18 & grid_cost$y==5)], 2)),pos=3)
ltext(18,5,cex=1.2,paste0("no. plots =",ceiling(grid_nsamp$z[which(grid_cost$x==18 & grid_cost$y==5)])),pos=3)
lpoints(18,4,pch=16)
ltext(17,3.2,cex=1.2,paste0("no. weeks =",round(grid_cost$z[which(grid_cost$x==18 & grid_cost$y==4)], 2)),pos=3)
ltext(17,2.8,cex=1.2,paste0("no. plots =",ceiling(grid_nsamp$z[which(grid_cost$x==18 & grid_cost$y==4)])),pos=3)
lpoints(20,4,pch=16)
ltext(20.5,3.2,cex=1.2,paste0("no. weeks =",round(grid_cost$z[which(grid_cost$x==20 & grid_cost$y==4)], 2)),pos=3)
ltext(20.5,2.8,cex=1.2,paste0("no. plots =",ceiling(grid_nsamp$z[which(grid_cost$x==20 & grid_cost$y==4)])),pos=3)
trellis.unfocus() 

n_plots_final<-ceiling(grid_nsamp$z[which(grid_nsamp$x==15 & grid_nsamp$y==5)])
grid_area<-sum(lib_area/100)/(n_plots_final*lib_area/lib_area)
grid_dist<-sqrt(sum(lib_area/100)/(n_plots_final*lib_area/lib_area))#[1] 18.34101
n_plots_final<-ceiling(grid_nsamp$z[which(grid_nsamp$x==18 & grid_nsamp$y==5)])
grid_area<-sum(lib_area/100)/(n_plots_final*lib_area/lib_area)
grid_dist<-sqrt(sum(lib_area/100)/(n_plots_final*lib_area/lib_area))#[1] 22.00458
n_plots_final<-ceiling(grid_nsamp$z[which(grid_nsamp$x==20 & grid_nsamp$y==4)])
grid_area<-sum(lib_area/100)/(n_plots_final*lib_area/lib_area)
grid_dist<-sqrt(sum(lib_area/100)/(n_plots_final*lib_area/lib_area))#[1] 21.83975


##
## GAEL: Playing with the optimization chunk
##

grid_nsubplots<-seq(1,6,0.1)
grid_radius<-seq(15,20,0.1)

grid_cost<-expand.grid(x=grid_radius,y=grid_nsubplots) 
grid_nsamp<-expand.grid(x=grid_radius,y=grid_nsubplots)

for (i in grid_nsubplots){#no of subplots
  for (j in grid_radius){#radius
    subplot_area_2= pi*j^2/10000 # ha. of subplot (to be used if calculations at subplot level)
    plot_area_2= subplot_area_2 * i #ha. of cluster (to be used if calculations at cluster level)
    AGB_1A_plot_2<-AGB_1A*plot_area_2
    Sh_1A_plot_2=scale_stdev(pix_area,plot_area_2,Sh_1A,0.5)  # stdev per ha in plot FMC
    strAlloc_1A_2<- strAlloc(Nh = Nh_1A/plot_area_2, Sh = Sh_1A_plot_2, CV0 = 0.1/stud_t_90, ch = T_1, ybarU=AGB_1A_plot_2,alloc = "totvar")#ybarU is population mean
    n_init_2<-strAlloc_1A_2$nh
    T_1_2<-Cost_plot_hours(n0=n_init_2,Nh=Nh_1A,AGB=AGB_1A,subplotsize=subplot_area_2,n_subplots=i,d_subplots=dist_sub,c=c_1,v=v_1,rho=rho_1,nu=nu_1,tp=tp_1) 
    #tot_hours_1team<-n_init*T_1#number of hours or whole inventory for one team
    tot_weeks_6team<-n_init_2*T_1_2/(n_teams*n_hours_day*n_days_week)#number of months or whole inventory for six team
    id<-which(grid_cost$x==j & grid_cost$y==i)
    grid_cost$z[id]<-ceiling(tot_weeks_6team)
    grid_nsamp$z[id]<-ceiling(n_init_2)
  }
}




## GAEL: testing different vizu: contours is simpler but not as nice
library(tidyr)
library(dplyr)
library(ggplot2)


grid_nsamp2 <- grid_nsamp %>%
  filter(z >= 100 , z < 700)

ggplot(grid_cost, aes(x = x, y = y)) +
  geom_contour_filled(aes(z = z)) +
  geom_contour(data = grid_nsamp2, aes(z = z, colour = after_stat(level)), binwidth = 50, size = 1) +
  scale_color_gradient2(low = "darkgreen", high = "darkred", mid = "yellow", midpoint = 400) +
  labs(fill = "N weeks", color = "N plot")



## GAEL : otherwise manual plot:

# n400 <- grid_nsamp %>% filter(z == 400)
# n350 <- grid_nsamp %>% filter(z == 350)
# n300 <- grid_nsamp %>% filter(z == 300)
# n250 <- grid_nsamp %>% filter(z == 250)
# n200 <- grid_nsamp %>% filter(z == 200)
# 
# n40 <- grid_cost %>% filter(z == 40)
# n35 <- grid_cost %>% filter(z == 35)
# n30 <- grid_cost %>% filter(z == 30)
# n25 <- grid_cost %>% filter(z == 25)
# n20 <- grid_cost %>% filter(z == 20)
# 
# start_point <- tibble(x = 17.84, y = 4) %>%
#   left_join(grid_nsamp, by = c("x", "y")) %>%
#   left_join(grid_cost, by = c("x", "y"), suffix = c("_plot", "_weeks"))
# 
# 
# ## Make graph by hand
# ggplot(grid_cost, aes(x = x, y = y)) +
#   
#   ## Heat map based on cost 
#   ## !!! Too heavy
#   # geom_tile(aes(fill = z)) +
#   # scale_fill_viridis_c() +
#   
#   ## Curves of iso n plots
#   geom_smooth(data = n400, color = "darkred") +
#   geom_label(data = filter(n400, x == max(x), y == min(y)), aes(label = z)) +
#   geom_smooth(data = n350, color = "red") +
#   geom_label(data = filter(n350, x == max(x), y == min(y)), aes(label = z)) +
#   geom_smooth(data = n300, color = "darkorange") +
#   geom_label(data = filter(n300, x == max(x), y == min(y)), aes(label = z)) +
#   geom_smooth(data = n250, color = "orange") +
#   geom_label(data = filter(n250, x == max(x), y == min(y)), aes(label = z)) +
#   geom_smooth(data = n200, color = "gold") +
#   geom_label(data = filter(n200, x == max(x), y == min(y)), aes(label = z)) +
#   
#   ## Example point
#   geom_point(data = start_point, size = 2, color = "red") +
#   annotate(
#     geom = "text", x = start_point$x + .4, y = start_point$y, 
#     label = paste0("N plots: ", start_point$z_plot, "\nN weeks: ", start_point$z_weeks)
#     ) +
#   
#   ## Curves of iso n weeks
#   geom_smooth(data = n40, color = "darkblue") +
#   geom_label(data = filter(n40, x == min(x), y == max(y)), aes(label = z)) +
#   geom_smooth(data = n35, color = "blue") +
#   geom_label(data = filter(n35, x == min(x), y == max(y)), aes(label = z)) +
#   geom_smooth(data = n30, color = "darkcyan") +
#   geom_label(data = filter(n30, x == min(x), y == max(y)), aes(label = z)) +
#   geom_smooth(data = n25, color = "cyan") +
#   geom_label(data = filter(n25, x == min(x), y == max(y)), aes(label = z)) +
#   geom_smooth(data = n20, color = "green") +
#   geom_label(data = filter(n20, x == min(x), y == max(y)), aes(label = z))






## GAEL: Commented out I have other methods to make hexagonal grid

# #METHOD 2: #Get a hexagonal grid that covers Lberia
# 
# library(dplyr)
# library(tidyr)
# library(sp)
# library(raster)
# library(rgeos)
# #library(rgbif)
# # library(viridis)
# # library(gridExtra)
# # library(rasterVis)
# # library(lattice)
#  set.seed(1)
# # library(dggridR)
#  library(REdaS)#convert radiand to degs..
# 
# 
# #Study region
# #THE OPERATOR %>% ESTABLISHED A PIPELINE OF COMMANDS
# # disaggregate disaggregates raster to higher resolution 
# # geomtery converts a data frame into a spatial object
#  #If FAO laptop
# # study_area <- getData("GADM", country = "LR", level = 0, 
# #                       path = "C:/Users/garciaperezj/Desktop/Scripts/HexagonalGrid") %>% 
# #   disaggregate %>% 
# #   geometry
# #If Javier laptop
# study_area <- getData("GADM", country = "LR", level = 0, 
#                       path = "C:/Users/javier/Desktop/Collect/Liberia_March2018/HexagonalGrid") %>% 
#   disaggregate %>% 
#   geometry
# #slot returns slots from an object (equivalent to bin borders)
# study_area <- sapply(study_area@polygons, slot, "area") %>% 
#   {which(. == max(.))} %>% 
#   study_area[.]
# #study_area<-crop(study_area, extent(79.5, 82, 6, 10))
# ## Create the clipping polygon
# #CP <- as(extent(79.5, 82, 6, 10), "SpatialPolygons")
# #proj4string(CP) <- CRS(proj4string(study_area))
# ## Clip the map
# #out <- gIntersection(study_area, CP, byid=TRUE)
# 
# plot(study_area, col = "grey50", bg = "light blue",axes=TRUE)
# text(-12, 5, "Study Area:\nLiberia")
# 
# #Creating grids
# #Hexagonal grids
# set.seed(5)
# size <- 0.179#This is for 285 points. play with this value until it gives you the right number of points (in our case for Liberia, 285 points)
# #size<-0.09#1114 points
# #size<-0.04#5657 points
# #size <- 0.2117#This for 198 points
# #size<-0.2107#This for 201 points
# hex_points <- spsample(study_area, type = "hexagonal", cellsize = size)
# hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = size)
# #plot(study_area,main="15 m radius, 5 subplots", col = "grey50", bg = "light blue", axes = TRUE)
# #plot(study_area,main="18 m radius, 5 subplots", col = "grey50", bg = "light blue", axes = TRUE)
# plot(study_area,main="20 m radius, 4 subplots", col = "grey50", bg = "light blue", axes = TRUE)
# plot(hex_points, col = "black", pch = 20, cex = 0.5, add = T)
# plot(hex_grid, border = "orange", add = T)
# hex_points_df <- SpatialPointsDataFrame(coords=coordinates(as.data.frame(hex_points)),data=as.data.frame(hex_points))
# proj4string(hex_points_df) <- CRS("+init=epsg:4326")#epsg:4236 is the international degrees projection
# #Convert spatialPolygons to SpatialPolygonsdataframe
# hex_grid_df<- data.frame(id = getSpPPolygonsIDSlots(hex_grid))
# row.names(hex_grid_df) <- getSpPPolygonsIDSlots(hex_grid)
# # Make spatial polygon data frame
# hex_grid_df <- SpatialPolygonsDataFrame(hex_grid, data =hex_grid_df)
# proj4string(hex_grid_df) <- CRS("+init=epsg:4326")#epsg:4236 is the international degrees projection
# 
# #investigate distance between points
# # The area of an hexagon:
# area_hex<-area_ha/10260
#   length(hex_points) #no. of points from summary(hex_points)
# bas<-sqrt(2*area_hex/(3*sqrt(3)))#side of hexgon, based on area= (3/2)*sqrt(3)*bas^2-> bas=sqrt(2*area)/(3*sqrt(3))
# heig<-2*(area_hex/6)/bas #distance from center to mid side, based on area= base * heght/2
# dist_cent<-heig*2/10 #(units of 100 meters translated to kilometers)
# #######################
# 
# 
# 
# #write the spatial dataframe objects to shapefiles
# #writeOGR(hex_points_df,'C:/Users/garciaperezj/Desktop/Liberia/LiberiaMissionFeb2018','ClusterPlots', 
# #         driver="ESRI Shapefile",overwrite_layer=TRUE)
# #writeOGR(hex_grid_df,'C:/Users/garciaperezj/Desktop/Liberia/LiberiaMissionFeb2018','HexagonGrid', 
# #         driver="ESRI Shapefile",overwrite_layer=TRUE)
# #writeOGR(hex_points_df, dsn='C:/Users/garciaperezj/Desktop/Liberia/LiberiaMissionFeb2018/ClusterPlots.kml', layer= 'ClusterPlots', driver="KML")#kml file
