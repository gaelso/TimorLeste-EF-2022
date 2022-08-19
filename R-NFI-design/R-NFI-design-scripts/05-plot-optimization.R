
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

