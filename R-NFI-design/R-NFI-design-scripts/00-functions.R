
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
        scale_fill_manual(values = forest_colors) +
        theme(text = element_text(family = "LoraIt")) +
        ggspatial::annotation_scale(
          location = "tr",
          bar_cols = c("grey60", "white"),
          text_family = "LoraIt"
        ) +
        ggspatial::annotation_north_arrow(
          location = "tr", 
          which_north = "true",
          pad_x = unit(0.2, "in"), 
          pad_y = unit(0.3, "in"),
          style = ggspatial::north_arrow_nautical(
            fill = c("grey40", "white"),
            line_col = "grey20",
            text_family = "LoraIt"
          )
        )
      
    }
    
  }
  
  if (is.null(raster)) {
    
    list(grid = sf_grid, points = sf_points, graph = gr_grid) 
    
  } else {
    
    list(
      grid = sf_grid2, points = sf_points, graph = gr_grid, plot = sf_plot, 
      n_plot = n_plot, n_plot_forest = n_plot_forest, gr_forest = gr_forest
    )
    
  } ## END if
  
} ## END function



## + Time required for measurements ----

## Based on Picard 2007 (https://core.ac.uk/download/pdf/52632663.pdf)
## total_time = n_plot * (unit_time_measure + unit_time_travel)
## time_travel = 1 / march_speed * sqrt(area_forest / n_plot)
## time_measure = time_measure + time_delineate
##              = area_plot * unit_time_measure + plot_perimeter * unit_time_delineate
##              = area_plot * unit_time_measure + 2 * sqrt(pi * area_plot) * unit_time_delineate
## Adapted to NFI, plots become subplots and need to add local authorization and travel to plots:
##  - time_travel_subplots = average_distance * subplot_count / march_speed
##    + For L shaped plot, average_distance = subplot_distance * (subplot_count - 1) * 2 / subplot_count
##  - time_travel_plot = sqrt(area_country / n_plot) / car_speed ## Formula to convert nb of plots to grid spacing.
##    + Can be improved with average transportation from lodging to plot + transportation from office to lodging every week or two.
##  - time_authorization = time to get authorization from local village and recruit workers if necessary
calc_time <- function(unit_times, plot_design, nest_design = NULL, area_country, n_plot) {
  
  time_travel_plot    <- sqrt(area_country / n_plot) / unit_times$car_speed
  
  time_travel_subplot <- plot_design$subplot_avg_distance * plot_design$subplot_count / (unit_times$march_speed * 1000)
  
  
  if (!is.null(nest_design)) {
    
    ## Calculate time to measure for each nested level
    time_measure_plot   <- sum(nest_design$time_measure) * plot_design$subplot_count
    time_delineate_plot <- 0 
    
  } else {
    
    ## If not nested, using unit_times params and adding time to delineate plot
    time_measure_plot   <- plot_design$subplot_area * plot_design$subplot_count * 100^2 * unit_times$unit_time_measure
    time_delineate_plot <- plot_design$subplot_count * 2 * sqrt(pi * plot_design$subplot_area * 100^2) * unit_times$unit_time_delineate
    
  }
  
  time_authorization  <- unit_times$unit_time_authorization
  
  plot_time          <- time_travel_plot + time_travel_subplot + time_delineate_plot + time_measure_plot + time_authorization
  
  tibble(
    plot_time = plot_time,
    time_travel_plot = time_travel_plot,
    time_travel_subplot = time_travel_subplot,
    time_delineate_plot = time_delineate_plot,
    time_measure_plot = time_measure_plot, 
    time_authorization = time_authorization,
    nested_optimization = if_else(!is.null(nest_design), "yes", "no")
    )

} ## End function calc_time()



## + Optimize design ----

## Function to calculate NFI design for all combinations of user input plot 
## parameters. Steps:
## - Select a set of parameters and add subplot areas and distance
## - Calc CV to calculate the expected CV based on input parameters
## - Use calc_time() to calculate the time cost of measuring each plot based on input parameters
optimize_design <- function(params_input, nest_input, unit_times, CV_input, area_input){
  
  ## 1. Make a table of unique param combinations and add subplot characteristics
  ## !!! Average distance only accept "L" plot shape for now but structure ready for the other shapes
  params_combi <- expand.grid(params_input) %>%
    as_tibble(.) %>%
    mutate(across(where(is.double), as.integer)) %>%
    mutate(across(where(is.factor), as.character)) %>%
    mutate(id = 1:nrow(.)) %>%
    select(id, everything()) %>%
    mutate(
      subplot_area     = round(pi * nest1_radius^2 /100^2, 3),
      subplot_distance = distance_multiplier * nest1_radius,
      subplot_avg_distance = case_when(
        subplot_count == 1 ~ subplot_distance,
        plot_shape == "L"  ~ as.integer(subplot_distance * (subplot_count - 1) * 2 / subplot_count),
        TRUE ~ NA_integer_
      )
    )
  
  ## 2. Calculate CV and cost for all combinations of input parameters
  results_combi <- map(1:nrow(params_combi), function(x){
    
    if (x == round(x/50)*50) print(paste("calculating combination ", x, "out of ", nrow(params_combi)))
    
    ## 2.1 Select one set of params  
    params <- params_combi %>% slice(x)
    
    ## 2.2 Complete nested design params
    if (is.na(params$nest2_radius)) {
      nest_design <- NULL
    } else {
      nest_design <- tibble(
        nest_level = c("nest1", "nest2", "nest3"),
        nest_radius = c(params$nest1_radius, params$nest2_radius, 2.5)
      ) %>%
        left_join(nest_input, by = "nest_level") %>%
        mutate(
          id = params$id,
          nest_area         = round(pi * nest_radius^2 / 100^2, 3),
          unit_time_measure = tree_density * unit_time_measure / (60 * 100^2), ## h/m2
          time_measure      = nest_area * 100^2 * unit_time_measure  ## h
        ) %>%
        select(id, everything())
    } ## End if
    
    ## 2.3 Calc CV 
    CV_output <- CV_input %>%
      mutate(
        CV_init_area_corr = if_else(CV_init_area > 1, 1, CV_init_area), ## assumption that CV is constant after 1 ha
        CV_area = params$subplot_area * params$subplot_count,
        CV = sqrt(CV_init^2 * (CV_init_area_corr / CV_area)^0.5) ## Lynch 2017 https://academic.oup.com/forestry/article/90/2/211/2605853
      )
    
    ## 2.3 Calc number of plots
    n_plot <- round((CV_output$CV * qt(.95, df=Inf) / params$allowable_error)^2)
    
    grid_spacing <- ceiling(sqrt(area_input$area_forest / n_plot))
    
    ## 2.4 Calc time to measure one plot
    plot_time <- calc_time(
      unit_times = unit_times, 
      plot_design = params, 
      nest_design = nest_design, 
      area_country = area_input$area_country, 
      n_plot = n_plot
    ) %>%
      mutate(id = params$id) %>%
      select(id, everything())
    
    ## 2.5 Total time cost
    total_time <- plot_time$plot_time * n_plot / (unit_times$working_hours * unit_times$working_days)
    
    ## 2.6 Output
    list(
      params = params %>%
        mutate(CV = CV_output$CV, n_plot = n_plot, grid_spacing = grid_spacing, plot_time = plot_time$plot_time, total_time = total_time  
        ),
      plot_time = plot_time,
      nest_time = nest_design
    )
    
  }) ## End optimization loop
  
  ## 3. Arrange outputs
  params_combi <- map_dfr(1:nrow(params_combi), function(i){
    results_combi[[i]]$params
  })
  
  plot_times_combi <- map_dfr(1:nrow(params_combi), function(i){
    results_combi[[i]]$plot_time
  })
  
  nest_design_combi <- map_dfr(1:nrow(params_combi), function(i){
    results_combi[[i]]$nest_time
  })
  
  ## Calculate 10 best for cost and CV
  params_filter <- params_combi %>%
    filter(total_time <= 24, plot_time <= 15, allowable_error == 10)
  
  best_CV <- params_filter %>%
    arrange(CV) %>%
    slice_head(n = 10) %>%
    mutate(type = "Lowest CV") %>%
    select(type, everything())
  
  best_time <- params_filter %>%
    arrange(total_time) %>%
    slice_head(n = 10) %>%
    mutate(type = "Lowest cost") %>%
    select(type, everything())
  
  solutions <- bind_rows(best_CV, best_time)
  
  ## 4. Outputs
  list(
    params = params_combi,
    plot_time = plot_times_combi,
    nest_design = nest_design_combi,
    solutions = solutions
  )
  
} ## End function optimize design


