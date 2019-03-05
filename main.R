library(tidyverse)
library(sf)
library(raster)
library(units)

# Read mine locations and deforestation layer
mines <- sf::st_read("./input/snl_samples.shp")
deforestation <- raster::raster("./input/Hansen_GFC-2017-v1.5_lossyear_00N_060W.tif")

# Reproject vector and raster
mines_reproject <- sf::st_transform(mines, crs = "+proj=robin")
deforestation_reproject <- deforestation %>% raster::projectRaster(crs = "+proj=robin", 
                                                 method = "ngb")

# Plot data check
image(deforestation_reproject)
plot(mines_reproject, add = TRUE, col = "black")

# Buffer mines (10 km) and merge overlapping ones; keep attributes(!)
buffer <- units::set_units(20, km)
mines_reproject %>% sf::st_buffer(dist = buffer) %>%
  sf::st_union() %>% 
  sf::st_cast("POLYGON") %>% 
  sf::st_sf() %>% 
  dplyr::mutate(id = 1:n())


plot(mines_reproject)
?st_union
