
## +++ +++
## NFI design optimization scripts for Timor Leste
## +++ +++


## + Make NFI grids ----

## Build grid and check number of forest plots if a LC raster file is provided
make_grid <- function(spacing_km = 10, offset = NULL, square = FALSE, raster = rs_jica, forest_classes = NULL, forest_colors = NULL){
  
  ## grid
  sf_grid <- st_make_grid(
    x = sf_country, 
    cellsize = c(spacing_km * 1000, spacing_km * 1000), 
    what = "polygons", 
    square = square, 
    offset = offset
  ) %>%
    st_as_sf()
  
  sf_points <- st_make_grid(
    x = sf_country, 
    cellsize = c(spacing_km * 1000, spacing_km * 1000),  
    what = "centers", 
    square = square, 
    offset = offset
  ) %>%
    st_as_sf()
  
  ## intersect actual country boundaries
  sf_points2 <- sf_points[sf_country, ] 
  
  sf_grid2 <- sf_grid[sf_points2, ]
  
  ## Graph
  gr_grid <- ggplot() +
    geom_sf(data = sf_country, fill = NA, size = 1) +
    geom_sf(data = sf_grid2, fill = NA, color = "red") +
    geom_sf(data = sf_points2, size = 0.5)
  
  
  ## Add Land Cover and calculate number of plots per LC class
  if(!is.null(raster)) {
    
    sf_plot <- terra::extract(raster, vect(sf_points2))
    
    sf_points3 <- sf_points2 %>%
      bind_cols(sf_plot) %>%
      filter(lc %in% forest_classes)
    
    sf_grid3 <- sf_grid2 %>%
      bind_cols(sf_plot) %>%
      filter(lc %in% forest_classes)
    
    n_plot <- sf_plot %>%
      as_tibble() %>%
      group_by(lc) %>%
      summarise(n = n())
    
    n_plot_forest <- sf_points3 %>%
      as_tibble() %>%
      summarise(n = n())
    
    
    gr_grid2 <- ggplot() +
      geom_sf(data = sf_country, fill = NA, size = 1) +
      geom_sf(data = sf_grid3, aes(fill = lc), color = NA) +
      geom_sf(data = sf_grid2, fill = NA, color = "darkorange") +
      labs(fill = NULL) +
      theme(legend.position = "bottom")
    
    if(is.null(forest_colors)) {
      
      gr_forest <- gr_grid2 +
        scale_color_viridis_d()
      
    } else {
      
      gr_forest <- gr_grid2 +
        scale_fill_manual(values = forest_colors)
      
    }
    
  }
  
  if (is.null(raster)) {
    
    list(grid = sf_grid, points = sf_points, graph = gr_grid) 
    
  } else {
    
    list(
      grid = sf_grid, points = sf_points, graph = gr_grid, plot = sf_plot, 
      n_plot = n_plot, n_plot_forest = n_plot_forest, gr_forest = gr_forest
    )
    
  } ## END if
  
} ## END function



## + Time required for measurements ----

calc_time <- function(n0, Nh, AGB, subplot_area, n_subplots, d_subplots, 
                      c=NULL, v=NULL, rho=NULL, nu=NULL, tp=NULL) {
  ## calculation of costs (from Picard's Guide methodologique d'evaluation rapide des bois)
  
  ## n0            Number of plots
  ## Nh            Population area
  ## subplot_area  Area of subplot in ha
  ## n_subplot     Number of subplots
  ## d_subplot     Distance between subplots. #1000 in denominator transforms to km
  ## c
  ## v
  ## rho
  ## nu
  ## tp 
  
  ## tw = Time to walk from plot to plot. It assumes n0=400 plots. Areas are converted to km here.
  ## Walking assumes a cross where team has to go back to center subplot for every side of the cross
  tw <- if (!is.null(v)) d_subplots * (n_subplots - 1) * 2 / (1000 * v) else rep(0, length(Nh))
  
  ## tc = time to drive from plot to plot. It assumes n0=400 plots. Areas are converted
  #to km here
  tc<-if (!is.null(c)) sqrt(sum(Nh / 100) / (n0 * Nh / sum(Nh))) / c else rep(0,length(Nh))
  
  ## tm = time to measure plot proportional to area and plot biomass
  tm <- if (!is.null(rho)) rho * area_subplot * no_subplots * 10000 * AGB / max(AGB) else rep(0, length(AGB))
  
  #tm<-if(!is.null(rho)) rho*subplotsize*no_subplots*10000*AGB else rep(0,length(AGB))
  
  ## time to delimitate plot proportional to perimeter and weighted with plot biomass
  td <- if (!is.null(nu)) 2 * nu * pi * sqrt(subplotsize * 10000 / pi) * no_subplots * sqrt(AGB) / max(sqrt(AGB)) else rep(0, length(AGB))
  
  #time to request permission
  tp<-if(!is.null(tp)) tp else rep(0,length(Nh))
  ##Total cost per plot
  T<-tc+tw+tm+td+tp
  #  return(T)
}