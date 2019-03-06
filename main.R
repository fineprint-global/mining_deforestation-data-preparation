library(tidyverse)
library(sf)
library(raster)
library(units)

# Read mine locations and deforestation layer
mines <- sf::st_read("./input/snl_samples.shp")
deforestation <- raster::raster("./input/Hansen_GFC-2017-v1.5_lossyear_00N_060W.tif")
view(mines)
view(deforestation)

# Plot deforestation and mines without reproject
image(deforestation)
plot(mines, add = TRUE, col = "black")

# Reproject vector and raster
mines_reproject <- sf::st_transform(mines, crs = "+proj=robin")
deforestation_reproject <- deforestation %>% raster::projectRaster(crs = "+proj=robin", 
                                                 method = "ngb")
view(mines_reproject)

#######################################################################################
# Plot reprojected data
image(deforestation_reproject)
plot(mines_reproject, add = TRUE, col = "black")

# Create Buffer Areas of mines (20 km) and merge overlapping areas; trace back to mines
buffer <- units::set_units(20, km)
mines_reproject_buffer <- sf::st_buffer(mines_reproject, dist = buffer, nQuadSegs = 30, endCapStyle = "ROUND",
                                  joinStyle = "ROUND", mitreLimit = 1) %>%  
  sf::st_union() %>%  
  sf::st_cast("POLYGON")  %>% 
  sf::st_sf() %>% 
  dplyr::mutate(id_buffer = 1:n())

view(st_join(mines_reproject, mines_reproject_buffer, join = st_intersects))

mines_buffer_test <- mines_reproject %>% 
  dplyr::mutate(year = raster::extract(deforestation_reproject, as(mines_reproject, "Spatial")))
mines_buffer_test
view(mines_buffer_test)

mines_buffer_test <- mines_reproject_buffer %>% 
  dplyr::mutate(year = raster::extract(deforestation_reproject, as(mines_reproject_buffer, "Spatial")))

mines_buffer_test$year[[1]]
view(mines_buffer_test)


mines_buffer_test <- mines %>% 
  dplyr::mutate(year = raster::extract(deforestation_year, as(mines, "Spatial"), buffer = 500))


