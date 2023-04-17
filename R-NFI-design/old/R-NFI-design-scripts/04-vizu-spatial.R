
## +++ +++
## NFI design optimization scripts for Timor Leste
## +++ +++



# ## + ggplot ----
# 
# ggplot() +
#   geom_raster(data = df_jica, aes(x = x, y = y, fill = lc)) +
#   scale_fill_manual(values = jica_lc$hex) +
#   geom_sf(data = sf_points) +
#   geom_sf(data = sf_grid, fill = NA, col = "red", size = 0.1) +
#   geom_sf(data = sf_country, fill = NA, col = "red", size = 1)
# 
# # ggplot() +
# #   geom_raster(data = df_jica, aes(x = x, y = y, fill = lc)) +
# #   scale_fill_manual(values = jica_lc$hex) +
# #   coord_fixed()
# 
# ggplot() +
#   geom_tile(data = df_jica, aes(x = x, y = y, fill = lc)) +
#   scale_fill_manual(values = jica_lc$hex) +
#   coord_fixed()


## Leaflet ----

tmap_mode("view")

#tm_basemap("OpenTopoMap") +
#tm_basemap("GeoportailFrance.orthos") +
tm_basemap("Esri.WorldImagery") +
  tm_shape(sf_plot) + tm_dots(col = "forest_type", title = "Forest type", size = 0.1) +
  #tm_shape(sf_points) + tm_dots(size = 0.02, col = "darkred") +
  tm_shape(sf_random) + tm_dots(size = 0.02, col = "purple") +
  tm_shape(sf_grid) + tm_borders(col = "red") +
  tm_shape(sf_country) + tm_borders(col = "red") +
  tm_shape(rs_jica) + tm_raster(style = "cont", palette = jica_lc$hex, legend.show = TRUE)





# ggplot() +
#   geom_stars(data = rs_jica2)