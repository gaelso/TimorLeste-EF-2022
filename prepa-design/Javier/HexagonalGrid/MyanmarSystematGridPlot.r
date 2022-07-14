## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# PLOTTING SYSTEMATIC GRID IN MYANMAR
# Plots first the whole country at the region and division level with one district
# in yellow and a 8 x8 km grid on top. Then zooms in that district to draw that grid
# and a new grid effectively doubling the sampling intensity
## Calculation steps
## Javier Gacia Perez Gamarra, 2018
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# First one has to download gadm polygons from www.gadm.org/country.
# See also https://www.students.ncl.ac.uk/keith.newman/r/maps-in-r-using-gadm#gadm
## Load required packages
library(maptools)
library(raster)
usePackage("countrycode")
#GET COUNTRY CODE
# English to ISO
Myan_code<-countrycode('Myanmar', 'country.name', 'iso3c')
## Download data from gadm.org 
gadm3 <- getData("GADM", country=Myan_code, level=3)
gadm2 <- getData("GADM", country=Myan_code, level=2)
gadm1 <- getData("GADM", country=Myan_code, level=1)
gadm0 <- getData("GADM", country=Myan_code, level=0)


#or else if it had been downloaded previously
#gadm3 <- readRDS("/Users/javier/Downloads/MMR_adm3.rds") # Township
#gadm2 <- readRDS("/Users/javier/Downloads/MMR_adm2.rds") # District
#gadm1 <- readRDS("/Users/javier/Downloads/MMR_adm1.rds") # Region and Division level
#gadm0 <- readRDS("/Users/javier/Downloads/MMR_adm0.rds") # Whole country

# Plotting layers
plot(gadm3)
plot(gadm2)
plot(gadm1)
plot(gadm0,col="red")

unique(gadm2$NAME_2)#check the row corresponding to Hkamti district=38
# Define color palette to put Hkamti in yellow
myColours <- rep("transparent", 63)
myColours[38] <- "yellow"
# Check it
plot(gadm2, col = myColours, border = 'white')
plot(gadm1,add=T)

# Check zoom in Hkamti
plot(gadm2, col = myColours, border = 'darkgrey', xlim = c(94.38, 97.08), ylim = c(24.1, 27.38))
library("rgdal")
proj4string(gadm2)# gives units (no units in gadm polygons: It is WSG84: decimal degs)



### define SpatialGrid object
library(raster)
grid0<-as(extent(gadm0), "SpatialPoints")# OR ALSO 
#grid0 <- raster(extent(grid0_transformed))

#Transform projection to get the extent in meters
#First give the original projection of shapefile
#proj4string(grid)<-proj4string(gadm0)
proj4string(grid0)<-proj4string(gadm0)
proj4string(gadm0);proj4string(grid0)
## We need units in m. Project with EPSG for Myanmar obtained from https://epsg.io/
grid0_transformed <- spTransform(grid0, CRS("+init=epsg:32647"))
gadm0_transformed<-spTransform(gadm0, CRS("+init=epsg:32647"))
gadm1_transformed<-spTransform(gadm1, CRS("+init=epsg:32647"))
gadm2_transformed<-spTransform(gadm2, CRS("+init=epsg:32647"))

proj4string(gadm0_transformed );proj4string(grid0_transformed )# check they are same

gridPoints0<-rasterToPoints(grid0,spatial=TRUE)#spatial=TRUE convert to SpatialPointsDataFrame 
head(gridPoints0@coords)# check coords
gridPoints0@bbox # check bounding box
bbox(gadm0_transformed) # or in this way
# assign CRS/projection
proj4string(gridPoints0) <- proj4string(grid0_transformed)

gridPoints0df<-data.frame(lon=gridPoints0@coords[,1],lat=gridPoints0@coords[,2],id="A",stringAsFactors=F)
coordinates(gridPoints0df) <- ~ lon + lat
proj4string(gridPoints0df) <- proj4string(gadm0_transformed)
### define SpatialGrid object
bb <- bbox(gadm0_transformed)
cs <- c(1, 1)*8000  # cell size 6km x 6km (for illustration)
# 1 m = 1 m
cc <- bb[, 1] + (cs/2)  # cell offset
cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
grd
# cellcentre.offset 923018 129964
# cellsize           19685  19685
# cells.dim              8      8

sp_grd <- SpatialGridDataFrame(grd,
                               data=data.frame(id=1:prod(cd)),
                               proj4string=CRS(proj4string(gadm0_transformed)))
## overlap to check NA values. RUN only if necessary
#over(gridPoints0df,sp_grd)
library("lattice")
spplot(sp_grd, "id", colorkey=FALSE,col.regions="white",
       #SpatialPolygonsRescale(layout.north.arrow(), offset=c(0,150000),scale = 400),
       panel = function(...) {
         panel.gridplot(..., border="black")
         sp.polygons(gadm1_transformed)
         sp.polygons(gadm2_transformed,fill = myColours, col = "transparent")
         sp.points(gridPoints0df, cex=0.1)
         #panel.text(...)
       })


## PLOT NOW THE ZOOM TO HKAMTI DISTRICT
## Crop to the desired extent, then plot
Hkamti<-gadm2_transformed[gadm2_transformed$NAME_2=="Hkamti",]
gadm2_transformed_crop <- crop(gadm2_transformed, extent(bbox(Hkamti)[1,1], bbox(Hkamti)[1,2], bbox(Hkamti)[2,1], bbox(Hkamti)[2,2]))
bbcrop <- bbox(gadm2_transformed_crop)
cs <- c(1, 1)*8000  # cell size 8km x 8km (for illustration)
# 1 m = 1 m
cccrop <- bbcrop[, 1] + (cs/2)  # cell offset
cdcrop <- ceiling(diff(t(bbcrop))/cs)  # number of cells per direction
grdcrop <- GridTopology(cellcentre.offset=cccrop, cellsize=cs, cells.dim=cdcrop)
sp_grdcrop <- SpatialGridDataFrame(grdcrop,
                                   data=data.frame(id=1:prod(cdcrop)),
                                   proj4string=CRS(proj4string(gadm2_transformed_crop)))
myColourscrop <- rep("transparent", length(unique(gadm2_transformed_crop$NAME_2)))
myColourscrop[which(unique(gadm2_transformed_crop$NAME_2)=="Hkamti")] <- "yellow"
#grid points for new intensified inventory (offset by (cs/2))
gridPoints0dfnew<-data.frame(lon=gridPoints0@coords[,1]+(cs/2),lat=gridPoints0@coords[,2]+(cs/2),id="A",stringAsFactors=F)
coordinates(gridPoints0dfnew) <- ~ lon + lat

spplot(sp_grdcrop, "id", colorkey=FALSE,col.regions="white",
       #SpatialPolygonsRescale(layout.north.arrow(), offset=c(0,150000),scale = 400),
       panel = function(...) {
         panel.gridplot(..., border="black")
         sp.polygons(gadm2_transformed_crop,fill = myColourscrop, col = "black")
         sp.points(gridPoints0df, cex=0.1)
         sp.points(gridPoints0dfnew,cex=0.1,col="red")
         #panel.text(...)
       })

##CREATING GRID WITH INITIAL ANGLE DIFFERENT FROM 0
# Specify the grid.
##angles <- c(15, 105)         # Orientations (in degrees) of easting and northing 
##or, with random orientation
set.seed(17)
angles <- c(0, 90) + runif(1, min=-90, max=90)
length <- c(310, 310)        # Grid spacings (meters), east-west and north-south
origin <- c(1735000, 285000) # Grid origin coordinates (meters, projected)
nrows <- 53                  # Number of west-east strips
ncols <- 23                  # Number of north-south strips

# Create the points on the grid.
basis <- rbind(cos(angles * pi/180), sin(angles * pi/180)) %*% diag(length)
x <- as.vector(outer(1:ncols, 1:nrows, FUN=function(x,y) x-1))
y <- as.vector(outer(1:ncols, 1:nrows, FUN=function(x,y) y-1))
grid <- t(basis %*% rbind(x, y) + origin)

# Display the grid.
plot(grid, asp=1, pch=19, col="Blue", cex=2 * (nrows * ncols)^(-1/4))

# Output the grid.
write.table(grid, file="f:/temp/grid.txt", sep="\t", 
            row.names=FALSE, col.names=c("Easting", "Northing"))
