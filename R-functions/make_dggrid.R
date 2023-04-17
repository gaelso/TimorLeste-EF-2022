
## Create dggrid hexagons and centroids for the desired resolution (.res) within 
## the boundaries of the AOI (.sf_aoi) and one of the AOI attribute (.aoi_class)

## A class is needed to remove hexagons for which the centroid is outside of this class

## Rather than looking for spatial overlap, the function extract AOI class 
## at the centroid location and remove NAs.

#######
## FOR TESTING ONLY
# library(sf)
# library(dggridR)
# library(tidyverse)
# 
# load("data/list_island.Rdata")
# 
# sf_test <- list_island$Kalimantan %>%
#   mutate(island = "Kalimantan")
# 
# res <- 11
######

make_dggrid <- function(.sf_aoi = sf_test, .res = 9, .aoi_class = island, .extract_class = T, .msg = T){
  
  ## Make sure we use WGS84
  sf_aoi <- st_transform(.sf_aoi, 4326)
  
  aoi_class <- enquo(.aoi_class)
  
  ## FOR TESTING ONLY
  #aoi_class <- sym("lc2021")
  ##
  
  ## Initiate grid 
  if (.msg) {
    message("1/3 Initiate grid...")
    t1 <- Sys.time()
  }
  
  bbox_aoi <- st_bbox(sf_aoi)
  
  dggs <- dgconstruct(res = .res)
  
  dist <- dggridR:::GridStat_cellDistKM(dggs[['projection']], dggs[['topology']], dggs[['aperture']], .res)
  
  dist_deg <- round(dist / 111 * 0.75, 4) ## calc degree with some margin of error
  
  sf_grid_init <- dgrectgrid(dggs, minlat=floor(bbox_aoi$ymin), minlon=floor(bbox_aoi$xmin), maxlat=ceiling(bbox_aoi$ymax), maxlon=ceiling(bbox_aoi$xmax), cellsize = dist_deg)  ## bounding box for Timor Leste
  
  vec_grid_num <- st_intersection(sf_grid_init, sf_aoi) %>% pull(seqnum)
  
  sf_grid_large <- sf_grid_init %>% filter(seqnum %in% vec_grid_num)
  
  gr_grid_large <- ggplot() +
    geom_sf(data = sf_aoi, fill = "grey80", color = NA) +
    geom_sf(data = sf_grid_large, fill = NA, color = "darkred")
  
  if (.msg) {
  t2       <- Sys.time()
  diffmin  <- round(as.numeric(t2 - t1, units = "mins"))
  diffsec  <- round(as.numeric(t2 - t1, units = "secs")) - diffmin * 60
  time_msg <- if_else(diffmin == 0, paste0(diffsec, " sec."), paste0(diffmin, " min ", diffsec, "sec."))
  message("...Done - ", time_msg)
  }
  
  
  ## Reduce grid
  if (.msg) {
    message("2/3 Reduce grid to target class...")
    t1 <- Sys.time()
  }
  
  sf_point <- st_centroid(sf_grid_large) %>% 
    st_join(sf_aoi) %>% 
    filter(!is.na(!!aoi_class)) %>%
    mutate(
      x = st_coordinates(.)[,1], 
      y = st_coordinates(.)[,2]
      )
  
  sf_grid <- sf_grid_large %>% filter(seqnum %in% sf_point$seqnum)
  
  ## Check grid
  gr_grid <- ggplot() +
    geom_sf(data = sf_aoi, fill = "grey80", color = NA) +
    geom_sf(data = sf_grid, fill = NA, color = "darkred") +
    geom_sf(data = sf_point, size = 0.6, color = "grey20")
  
  if (.msg) {
    print(gr_grid)
    t2       <- Sys.time()
    diffmin  <- round(as.numeric(t2 - t1, units = "mins"))
    diffsec  <- round(as.numeric(t2 - t1, units = "secs")) - diffmin * 60
    time_msg <- if_else(diffmin == 0, paste0(diffsec, " sec."), paste0(diffmin, " min ", diffsec, "sec."))
    message("...Done - ", time_msg)
  }
  
  ## Extract class if wanted
  if (.extract_class) {
    
    if(.msg) {
    message("3/3 Extract class info and make stats...")
    t1 <- Sys.time()
    }
    
    tab_class <- sf_point %>%
      as_tibble() %>%
      group_by(!!aoi_class) %>%
      summarise(count = n())
    
    gr_class <- ggplot() +
      geom_sf(data = sf_aoi, color = NA, aes(fill = !!aoi_class)) +
      geom_sf(data = sf_point, shape = 21, aes(fill = !!aoi_class))

    out <- list(grid = sf_grid, point = sf_point, gr_grid_large = gr_grid_large, 
                gr_grid = gr_grid, stat = tab_class, gr_class = gr_class)    
  
  } else {
    
    if (.msg) {
      message("3/3 Finalize outputs...")
      t1 <- Sys.time()
    }
    
    out <- list(grid = sf_grid, point = sf_point, gr_grid_large = gr_grid_large, gr_grid = gr_grid)
  
  }
  
  if (.msg) {
    t2       <- Sys.time()
    diffmin  <- round(as.numeric(t2 - t1, units = "mins"))
    diffsec  <- round(as.numeric(t2 - t1, units = "secs")) - diffmin * 60
    time_msg <- if_else(diffmin == 0, paste0(diffsec, " sec."), paste0(diffmin, " min ", diffsec, "sec."))
    message("...Done - ", time_msg)  
  }
  
  out
  
} ## END function
