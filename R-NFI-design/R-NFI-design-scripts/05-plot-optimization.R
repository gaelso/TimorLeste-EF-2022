
## +++ +++
## NFI design optimization scripts for Timor Leste
## +++ +++

## Calculation of CV and time for a number of plot size


## INPUT LIST:
## + Model params:
list_params <- list(
  subplot_count    = c(1, 3, 5),
  subplot_distance = c(2:4), ## multiplier of plot radius
  nest1_radius     = c(15:20),
  nest2_radius     = c(8:12),
  plot_shape       = c("L")
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
  march_speed = 2,              ## km/h
  car_speed = 10,               ## km/h
  unit_time_measure = 0.0035,   ## h/m^2 from Picard 2017
  unit_time_delineate = 0.0014, ## h/m    from Picard 2017
  unit_time_authorization = 2   ## h
)

## + country area
area_country <- 15000 ## ha


optimize_design <- function(list_params, nest_input, unit_times, area_country){
  
  ## 1. Make a table of unique param combinations
  input_optimization <- tidyr::expand_grid(
    list_params$subplot_count, 
    list_params$subplot_distance, 
    list_params$nest1_radius, 
    list_params$nest2_radius, 
    list_params$plot_shape
    ) %>%
    mutate(id = 1:nrow(.)) %>%
    select(id, everything())
  
  
  
  
  
} ## End function optimize design




result_optimization <- map_dfr(seq_along(input_optimization$id), function(x){
  
  ## Select row for calculations
  input <- input_optimization %>%
    slice(x)
  
  ## Calculate subplot characteristics
  subplot_area         <- round(pi * input$nest1_radius^2 /100^2, 3)
  subplot_distance     <- input$subplot_distance * input$nest1_radius
  
  if (input$subplot_distance == 1) {
    
    subplot_avg_distance <- subplot_distance
    
  } else {
    
    subplot_avg_distance <- case_when(
      input$plot_shape == "L" ~ subplot_distance * (input$subplot_count - 1) * 2 / input$subplot_count,
      TRUE ~ NA_real_
    )
    
  }
  
  ## Make nested level measurement times
  nest_design <- tibble(
    nest_level = c("nest1", "nest2", "nest3"),
    nest_radius = c(input$nest1_radius, input$nest2_radius, 2.5)
  ) %>%
    left_join(nest_input, by = "nest_level") %>%
    mutate(
      nest_area         = round(pi * nest_radius^2 / 100^2, 3),
      unit_time_measure = tree_density * unit_time_measure / (60 * 100^2), ## h/m2
      time_measure      = nest_area * 100^2 * unit_time_measure  ## h
      )
  
}) ## END MAP CALL


## Example values:
plot_design <- tibble(
  subplot_radius = 17.84, ## m
  subplot_count  = 5,      
  subplot_distance   = 60 ## m
) %>%
  mutate(
    subplot_area   = round(pi * subplot_radius^2 /100^2, 3),
    plot_area      = subplot_area * subplot_count,
    subplot_avg_distance_L  = subplot_distance * (subplot_count - 1) * 2 / subplot_count
  )

subplot_design <- tibble(
  nest_level = c("nest1", "nest2", "nest3"),
  subplot_radius = c(18, 12, 2.5),
  subplot_dbh_min = c(30, 10, 2),
  #tree_density    = c(200, 500, 1000)
  tree_density    = c(300, 1000, 1500)
) %>%
  mutate(subplot_area = round(pi * subplot_radius^2 / 100^2, 3))

unit_times <- tibble(
  march_speed = 2,              ## km/h
  car_speed = 10,               ## km/h
  unit_time_measure = 0.0035,   ## h/m^2 from Picard 2017
  unit_time_delineate = 0.0014, ## h/m    from Picard 2017
  unit_time_authorization = 2   ## h
)

unit_times_nest <- tibble(
  nest_level = c("nest1", "nest2", "nest3"),
  unit_time_measure = c(3, 2, 0.5)  ## in nb min /tree
)

nest_design <- subplot_design %>%
  left_join(unit_times_nest, by = "nest_level") %>%
  mutate(
    unit_time_measure = tree_density * unit_time_measure / (60 * 100^2), ## h/m2
    time_measure      = subplot_area * 100^2 * unit_time_measure  ## h
  ) 
nest_design

