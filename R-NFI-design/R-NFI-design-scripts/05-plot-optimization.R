
## +++ +++
## NFI design optimization scripts for Timor Leste
## +++ +++

## Calculation of CV and time for a number of plot size


## INPUT LIST:
## + Model params:
params_input <- list(
  subplot_count       = c(5),
  distance_multiplier = c(2), ## multiplier of plot radius as input for subplot distance
  nest1_radius        = c(18),
  nest2_radius        = c(10),
  plot_shape          = c("L"),
  allowable_error     = c(10)
)


params_input <- list(
  subplot_count       = c(1, 3, 5),
  distance_multiplier = c(2:4), ## multiplier of plot radius as input for subplot distance
  nest1_radius        = c(15:20),
  nest2_radius        = c(8:12),
  plot_shape          = c("L"),
  allowable_error     = c(1, 5, 10, 20)
)

## + Nested plot tree characteristics
nest_input <- tibble(
  nest_level = c("nest1", "nest2", "nest3"),
  nest_dbh_min = c(30, 10, 2),
  #tree_density    = c(200, 500, 1000)
  tree_density    = c(300, 1000, 1500),
  unit_time_measure = c(3, 2, 0.5)  ## in nb min /tree
)

## + unit times 
unit_times <- tibble(
  march_speed             = 2,      ## km/h
  car_speed               = 10,     ## km/h
  unit_time_measure       = 0.0035, ## h/m^2 from Picard 2017
  unit_time_delineate     = 0.0014, ## h/m    from Picard 2017
  unit_time_authorization = 2,      ## h
  working_hours           = 9,      ## h/day
  working_days            = 20,     ## days/month
)

## + Inputs for CV model
CV_input <- tibble(
  CV_init      = 66.3,  ## %
  CV_init_area = 84.3   ## ha, area of the plots used to calculate CV_init
)

## + Areas 
area_input <- list(
  area_country = 15000, ## ha
  area_forest  = 9323   ## ha
)


## Function to calculate NFI design for all combinations of user input plot 
## parameters. Steps:
## - Select a set of parameters and add subplot areas and distance
## - calc_CV() to calculate the expected CV based on input parameters
## - calc_time() to calculate the time cost of measuring each plot based on input parameters
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


res_opti <- optimize_design(
  params_input = params_input, 
  nest_input = nest_input, 
  unit_times = unit_times, 
  CV_input = CV_input, 
  area_input = area_input
  )

tt <- res_opti$solutions

# 
# 
# result_optimization <- map_dfr(seq_along(input_optimization$id), function(x){
#   
#   ## Select row for calculations
#   input <- input_optimization %>%
#     slice(x)
#   
#   ## Calculate subplot characteristics
#   subplot_area         <- round(pi * input$nest1_radius^2 /100^2, 3)
#   subplot_distance     <- input$subplot_distance * input$nest1_radius
#   
#   
#     
#   }
#   
#   ## Make nested level measurement times
#   nest_design <- tibble(
#     nest_level = c("nest1", "nest2", "nest3"),
#     nest_radius = c(input$nest1_radius, input$nest2_radius, 2.5)
#   ) %>%
#     left_join(nest_input, by = "nest_level") %>%
#     mutate(
#       nest_area         = round(pi * nest_radius^2 / 100^2, 3),
#       unit_time_measure = tree_density * unit_time_measure / (60 * 100^2), ## h/m2
#       time_measure      = nest_area * 100^2 * unit_time_measure  ## h
#       )
#   
# }) ## END MAP CALL
# 
# 
# ## Example values:
# plot_design <- tibble(
#   subplot_radius = 17.84, ## m
#   subplot_count  = 5,      
#   subplot_distance   = 60 ## m
# ) %>%
#   mutate(
#     subplot_area   = round(pi * subplot_radius^2 /100^2, 3),
#     plot_area      = subplot_area * subplot_count,
#     subplot_avg_distance_L  = subplot_distance * (subplot_count - 1) * 2 / subplot_count
#   )
# 
# subplot_design <- tibble(
#   nest_level = c("nest1", "nest2", "nest3"),
#   subplot_radius = c(18, 12, 2.5),
#   subplot_dbh_min = c(30, 10, 2),
#   #tree_density    = c(200, 500, 1000)
#   tree_density    = c(300, 1000, 1500)
# ) %>%
#   mutate(subplot_area = round(pi * subplot_radius^2 / 100^2, 3))
# 
# unit_times <- tibble(
#   march_speed = 2,              ## km/h
#   car_speed = 10,               ## km/h
#   unit_time_measure = 0.0035,   ## h/m^2 from Picard 2017
#   unit_time_delineate = 0.0014, ## h/m    from Picard 2017
#   unit_time_authorization = 2   ## h
# )
# 
# unit_times_nest <- tibble(
#   nest_level = c("nest1", "nest2", "nest3"),
#   unit_time_measure = c(3, 2, 0.5)  ## in nb min /tree
# )
# 
# nest_design <- subplot_design %>%
#   left_join(unit_times_nest, by = "nest_level") %>%
#   mutate(
#     unit_time_measure = tree_density * unit_time_measure / (60 * 100^2), ## h/m2
#     time_measure      = subplot_area * 100^2 * unit_time_measure  ## h
#   ) 
# nest_design

