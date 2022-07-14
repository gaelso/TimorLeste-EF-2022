##Generate hexagonal sample 
require(sp)
data(meuse.riv)
meuse.sr = SpatialPolygons(list(Polygons(list(Polygon(meuse.riv)), "x")))
plot(meuse.sr)

library(rgeos)
meuse.large = gBuffer(meuse.sr, width = 2000)
HexPts <-spsample(meuse.large, type="hexagonal", cellsize=1000)
HexPols <- HexPoints2SpatialPolygons(HexPts)
plot(HexPols[meuse.sr,], add=TRUE)

#Get a hexagonal grid that covers South Africa and Sri Lanka

# https://cran.r-project.org/web/packages/dggridR/vignettes/dggridR.html
# Advantages of hexagonal grids:
# http://strimas.com/spatial/hexagonal-grids/

#METHOD 1: #Get a hexagonal grid that covers South Africa
#NEEDS R 3.4!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(dggridR)

#Generate a dggs specifying an intercell spacing of ~25 miles
dggs      <- dgconstruct(type="ISEA3H",spacing=25, metric=FALSE, resround='nearest')
#dggs      <- dgconstruct(type="ISEA3H",spacing=18.34, metric=TRUE, resround='nearest')
#to set the right resolution check parameter res in help(dgconstruct)

#Use the included South Africa borders shapefile to generate a grid covering
#South Africa (including Lesotho - holes are not excluded)
#dg_shpfname_south_africa <- file.path(find.package('dggridR'), "extdata", "ZAF_adm0.shp")
dg_shpfname_south_africa <- function() {
  system.file("extdata", "ZAF_adm0.shp", package="dggridR")
}
sa_grid   <- dgshptogrid(dggs,dg_shpfname_south_africa())

#Read in the South Africa's borders from the shapefile
sa_border <- readOGR(dsn=dg_shpfname_south_africa, layer="ZAF_adm0")

#Plot South Africa's borders and the associated grid
p<- ggplot() + 
  geom_polygon(data=sa_border, aes(x=long, y=lat, group=group), fill=NA, color="black")   +
  geom_polygon(data=sa_grid,   aes(x=long, y=lat, group=group), fill="blue", alpha=0.4)   +
  geom_path   (data=sa_grid,   aes(x=long, y=lat, group=group), alpha=0.4, color="white") +
  coord_equal()
p

#METHOD 2: #Get a hexagonal grid that covers Sri Lanka

library(dplyr)
library(tidyr)
library(sp)
library(raster)
library(rgeos)
library(rgbif)
library(viridis)
library(gridExtra)
library(rasterVis)
library(lattice)
set.seed(1)
set.seed(2)
#Study region
#THE OPERATOR %>% ESTABLISHED A PIPELINE OF COMMANDS
# disaggregate disaggregates raster to higher resolution 
# geomtery converts a data frame into a spatial object
study_area <- getData("GADM", country = "LR", level = 0, 
                      path = "C:/Users/garciaperezj/Desktop/Scripts/HexagonalGrid") %>% 
  disaggregate %>% 
  geometry
#slot returns slots from an object (equivalent to bin borders)
study_area <- sapply(study_area@polygons, slot, "area") %>% 
  {which(. == max(.))} %>% 
  study_area[.]
#study_area<-crop(study_area, extent(79.5, 82, 6, 10))
## Create the clipping polygon
#CP <- as(extent(79.5, 82, 6, 10), "SpatialPolygons")
#proj4string(CP) <- CRS(proj4string(study_area))
## Clip the map
#out <- gIntersection(study_area, CP, byid=TRUE)

plot(study_area, col = "grey50", bg = "light blue",axes=TRUE)
text(-12, 5, "Study Area:\nLiberia")

#Creating grids
#Hexagonal grids
set.seed(5)
size <- 0.179#play with this value until it gives you the right number of points (in our case for Liberia, 285 points)
hex_points <- spsample(study_area, type = "hexagonal", cellsize = size)
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = size)
plot(study_area, col = "grey50", bg = "light blue", axes = TRUE)
plot(hex_points, col = "black", pch = 20, cex = 0.5, add = T)
plot(hex_grid, border = "orange", add = T)
hex_points_df <- SpatialPointsDataFrame(coords=coordinates(as.data.frame(hex_points)),data=as.data.frame(hex_points))
proj4string(hex_points_df) <- CRS("+init=epsg:4326")#epsg:4236 is the international degrees projection
#Convert spatialPolygons to SpatialPolygonsdataframe
hex_grid_df<- data.frame(id = getSpPPolygonsIDSlots(hex_grid))
row.names(hex_grid_df) <- getSpPPolygonsIDSlots(hex_grid)
# Make spatial polygon data frame
hex_grid_df <- SpatialPolygonsDataFrame(hex_grid, data =hex_grid_df)
proj4string(hex_grid_df) <- CRS("+init=epsg:4326")#epsg:4236 is the international degrees projection


#write the spatial dataframe objects to shapefiles
writeOGR(hex_points_df,'C:/Users/garciaperezj/Desktop/Liberia/LiberiaMissionFeb2018','ClusterPlots', 
         driver="ESRI Shapefile",overwrite_layer=TRUE)
writeOGR(hex_grid_df,'C:/Users/garciaperezj/Desktop/Liberia/LiberiaMissionFeb2018','HexagonGrid', 
         driver="ESRI Shapefile",overwrite_layer=TRUE)
writeOGR(hex_points_df, dsn='C:/Users/garciaperezj/Desktop/Liberia/LiberiaMissionFeb2018/ClusterPlots.kml', layer= 'ClusterPlots', driver="KML")#kml file


#spsample generates a different grid of points each time it's called because 
#the grid offset is chosen randomly by default. This can be fixed by setting 
#the offset parameter explicitly with offset = c(0, 0).
#Only cells whose centroid is fully within the study area polygon are created. 
#By buffering the study area it's possible to get full coverage by the grid, 
#which is usually what is desired.
#In some cases it may be desirable to clip the grid to the study area polygon 
#so that cells on the edge match the shape of the study area. This seems to 
#often be the case when setting up a grid of planning units for systematic 
#reserve design. For example, the official Marxan tutorial takes this approach. 
#Clipping can be performed using rgeos::gIntersection().
#The resolution of the grid is determined by the cellsize parameter, which is 
#the distance (dd) between centroids of neighbouring cells. Other ways of 
#defining cell size are the area (A), side length (s), or radius (r), and
#these are all related by:
#A=(3*sqrt(3)/2)*s^2=2*sqrt(3)*r^2=sqrt(3)/2*d^2

make_grid <- function(x, cell_diameter, cell_area, clip = FALSE) {
  if (missing(cell_diameter)) {
    if (missing(cell_area)) {
      stop("Must provide cell_diameter or cell_area")
    } else {
      cell_diameter <- sqrt(2 * cell_area / sqrt(3))
    }
  }
  ext <- as(extent(x) + cell_diameter, "SpatialPolygons")
  projection(ext) <- projection(x)
  # generate array of hexagon centers
  g <- spsample(ext, type = "hexagonal", cellsize = cell_diameter, 
                offset = c(0.5, 0.5))
  # convert center points to hexagons
  g <- HexPoints2SpatialPolygons(g, dx = cell_diameter)
  # clip to boundary of study area
  if (clip) {
    g <- gIntersection(g, x, byid = TRUE)
  } else {
    g <- g[x, ]
  }
  # clean up feature IDs
  row.names(g) <- as.character(1:length(g))
  return(g)
}
#generate a grid of 625km2 (25km by 25km) cells with and without
#clipping. This requires projecting the study area polygon to measure distance
#in kilometers.
# Find UTM zone here http://herpnet.org/herpnet/gbif/World_UTM_Map.pdf
study_area_utm <- CRS("+proj=utm +zone=29 +datum=WGS84 +units=km +no_defs") %>% 
  spTransform(study_area, .)
# without clipping
hex_grid <- make_grid(study_area_utm, cell_area = 625, clip = FALSE)
plot(study_area_utm, col = "grey50", bg = "light blue", axes = FALSE)
plot(hex_grid, border = "orange", add = TRUE)
box()
# with clipping
hex_grid <- make_grid(study_area_utm, cell_area = 625, clip = TRUE)
plot(study_area_utm, col = "grey50", bg = "light blue", axes = FALSE)
plot(hex_grid, border = "red", add = TRUE)
box()

#Square grid
r <- raster(study_area_utm, resolution = 25)
r <- rasterize(study_area_utm, r, field = 1)
plot(r, col = "grey50", axes = FALSE, legend = FALSE, bty="n", box=FALSE)
plot(study_area_utm, add = TRUE)
make_grid <- function(x, type, cell_width, cell_area, clip = FALSE) {
  if (!type %in% c("square", "hexagonal")) {
    stop("Type must be either 'square' or 'hexagonal'")
  }
  
  if (missing(cell_width)) {
    if (missing(cell_area)) {
      stop("Must provide cell_width or cell_area")
    } else {
      if (type == "square") {
        cell_width <- sqrt(cell_area)
      } else if (type == "hexagonal") {
        cell_width <- sqrt(2 * cell_area / sqrt(3))
      }
    }
  }
  # buffered extent of study area to define cells over
  ext <- as(extent(x) + cell_width, "SpatialPolygons")
  projection(ext) <- projection(x)
  # generate grid
  if (type == "square") {
    g <- raster(ext, resolution = cell_width)
    g <- as(g, "SpatialPolygons")
  } else if (type == "hexagonal") {
    # generate array of hexagon centers
    g <- spsample(ext, type = "hexagonal", cellsize = cell_width, offset = c(0, 0))
    # convert center points to hexagons
    g <- HexPoints2SpatialPolygons(g, dx = cell_width)
  }
  
  # clip to boundary of study area
  if (clip) {
    g <- gIntersection(g, x, byid = TRUE)
  } else {
    g <- g[x, ]
  }
  # clean up feature IDs
  row.names(g) <- as.character(1:length(g))
  return(g)
}
#plotting all four kinds of grid (clipped/non-clipped and hexagonal/square)
# hex - without clipping
hex_grid <- make_grid(study_area_utm, type = "hexagonal", cell_area = 625, clip = FALSE)
plot(study_area_utm, col = "grey50", bg = "light blue", axes = FALSE)
plot(hex_grid, border = "orange", add = TRUE)
box()
# hex - with clipping
hex_grid_c <- make_grid(study_area_utm, type = "hexagonal", cell_area = 625, clip = TRUE)
plot(study_area_utm, col = "grey50", bg = "light blue", axes = FALSE)
plot(hex_grid_c, border = "red", add = TRUE)
box()
# square - without clipping
sq_grid <- make_grid(study_area_utm, type = "square", cell_area = 625, clip = FALSE)
plot(study_area_utm, col = "grey50", bg = "light blue", axes = FALSE)
plot(sq_grid, border = "orange", add = TRUE)
box()
# square - with clipping
sq_grid_c <- make_grid(study_area_utm, type = "square", cell_area = 625, clip = TRUE)
plot(study_area_utm, col = "grey50", bg = "light blue", axes = FALSE)
plot(sq_grid_c, border = "red", add = TRUE)
box()

# Working with grids
#Once you've created a hexagonal grid, you'll likely want to aggregate some 
#data over the grid cells. Here I demonstrate three common aggregation tasks I
#often run into: aggregating points, polygons, or rasters
ecuador <- getData(name = "GADM", country = "ECU", level = 0, 
                   path = "C:/Users/garciaperezj/Desktop/Scripts/HexagonalGrid") %>% 
  disaggregate %>% 
  geometry
# exclude gapalapos
ecuador <- sapply(ecuador@polygons, slot, "area") %>% 
{which(. == max(.))} %>% 
  ecuador[.]
# albers equal area for south america
ecuador <- spTransform(ecuador, CRS(
  paste("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60",
        "+x_0=0 +y_0=0 +ellps=aust_SA +units=km +no_defs")))
hex_ecu <- make_grid(ecuador, type = "hexagonal", cell_area = 2500, clip = FALSE)
#Point density: bird observations in Ecuador from eBird.
ebird_key <- "4fa7b334-ce0d-4e88-aaae-2e0c138d049e"
bird_families <- c("Trochilidae", "Thraupidae", "Ardeidae", "Accipitridae")
families <- data_frame(family = bird_families) %>% 
  group_by(family) %>% 
  do(name_suggest(q = .$family, rank = "family")) %>% 
  filter(family == canonicalName) %>% 
  dplyr::select(family, key)
gb <- occ_search(taxonKey = families$key, country = "EC", datasetKey = ebird_key, 
                 limit = 3000, return = "data",
                 fields = c("family", "species", "decimalLatitude", "decimalLongitude"),
                 hasCoordinate = TRUE, hasGeospatialIssue = FALSE) %>% 
  bind_rows %>% 
  rename(lng = decimalLongitude, lat = decimalLatitude) %>% 
  as.data.frame
coordinates(gb) <- ~ lng + lat
projection(gb) <- projection(study_area)
gb <- spTransform(gb, projection(ecuador))
#Now I summarize these sightings over the hexagonal grid to get point density,
#and plot the data in the form of a heat map.
fill_missing <- expand.grid(id = row.names(hex_ecu), 
                            family = bird_families, stringsAsFactors = FALSE)
point_density <- over(hex_ecu, gb, returnList = TRUE) %>% 
  plyr::ldply(.fun = function(x) x, .id = "id") %>%
  mutate(id = as.character(id)) %>% 
  count(id, family) %>% 
  left_join(fill_missing, ., by = c("id", "family")) %>%
  # log transform
  mutate(n = ifelse(is.na(n), -1, log10(n))) %>% 
  spread(family, n, fill = -1) %>% 
  SpatialPolygonsDataFrame(hex_ecu, .)

spplot(point_density, bird_families,
       main = "Ecuador eBird Sightings by Family",
       col.regions = c("grey20", viridis(255)),
       colorkey = list(
         space = "bottom",
         at = c(-0.1, seq(0, log10(1200), length.out = 255)),
         labels = list(
           at = c(-0.1, log10(c(1, 5, 25, 75, 250, 1200))),
           labels = c(0, 1, 5, 25, 75, 250, 1200)
         )
       ),
       xlim = bbexpand(bbox(point_density)[1, ], 0.04), 
       ylim = bbexpand(bbox(point_density)[2, ], 0.04),
       par.strip.text = list(col = "white"),
       par.settings = list(
         strip.background = list(col = "grey40"))
)
#Polygon coverage
#extent to which grid cells are covered by a polygon geometry. This could be 
#in terms of absolute area covered or percent coverage. In the context of 
#systematic reserve design, we may have species ranges as polygons and want to
#know the amount of each grid cell that is suitable habitat for each species. 
#This can help highlight which cells are of highest conservation value.
#As a simple toy example, I use the boundary of Pastaza State.
pastaza <- getData(name = "GADM", country = "ECU", level = 1, 
                   path = "C:/Users/garciaperezj/Desktop/Scripts/HexagonalGrid") %>%
  subset(NAME_1 == "Pastaza") %>% 
  spTransform(projection(hex_ecu))
# cell areas
hex_area <- make_grid(ecuador, type = "hexagonal", cell_area = 2500, clip = TRUE)
hex_area <- gArea(hex_area, byid = T) %>% 
  data.frame(id = names(.), area = ., stringsAsFactors = FALSE) %>% 
  SpatialPolygonsDataFrame(hex_area, .)
hex_cover <- gIntersection(hex_area, pastaza, byid = TRUE) %>% 
  gArea(byid = TRUE) %>% 
  data.frame(id_both = names(.), cover_area = ., stringsAsFactors = FALSE) %>% 
  separate(id_both, "id", extra = "drop") %>% 
  merge(hex_area, ., by = "id")
hex_cover$cover_area[is.na(hex_cover$cover_area)] <- 0
hex_cover$pct_cover <- 100 * hex_cover$cover_area / hex_cover$area
# area
p1 <- spplot(hex_cover, "cover_area", col = "white", lwd = 0.5,
             main = expression(km^2),
             col.regions = plasma(256),
             par.settings = list(axis.line = list(col =  'transparent')),
             colorkey = list(
               space = "bottom",
               at = seq(0, 2500, length.out = 256),
               axis.line = list(col =  'black'))
)
# percent cover
p2 <- spplot(hex_cover, "pct_cover", col = "white", lwd = 0.5,
             main = expression("%"),
             col.regions = plasma(256),
             par.settings = list(axis.line = list(col =  'transparent')),
             colorkey = list(
               space = "bottom",
               at = seq(0, 100, length.out = 256),
               axis.line = list(col =  'black'))
)
grid.arrange(p1, p2, ncol = 2, top = "Ecuador: Coverage by Pastaza State")
#Raster aggregation
#aggregating raster layers over the grid cells. For example, elevation or
#climate variables in raster format might be averaged over hexagonal grid 
#cells, then used to parameterize a species distribution model.
srtm <- getData('alt', country = 'ECU', path = "C:/Users/garciaperezj/Desktop/Scripts/HexagonalGrid") %>% 
  projectRaster(t_crop, to = raster(hex_ecu, res=1)) %>% 
  setNames('elevation')
hex_srtm <- raster::extract(srtm, hex_ecu, fun = mean, na.rm = TRUE, sp = TRUE)
p1 <- rasterVis::levelplot(srtm, 
                col.regions = terrain.colors,
                margin = FALSE, scales = list(draw = FALSE),
                colorkey = list(
                  #space = "bottom",
                  at = seq(0, 6000, length.out = 256),
                  labels = list(at = 1000 * 0:6, 
                                labels = format(1000 * 0:6, big.mark = ","))
                )
)
p2 <- spplot(hex_srtm,
             col.regions = terrain.colors(256),
             at = seq(0, 4000, length.out = 256),
             colorkey = list(
               labels = list(at = seq(0, 4000, 500), 
                             labels = format(seq(0, 4000, 500), big.mark = ","))
             )
)
grid.arrange(p1, p2, ncol = 2, top = "Ecuador SRTM Elevation (m)")
