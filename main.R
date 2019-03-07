library(tidyverse)
library(sf)
library(raster)
library(units)

# Read mines und set buffersize in km
mines <- sf::st_read("./input/snl_samples.shp")
buffer <- units::set_units(20, km)

# Function, which  returns unprojected buffered mine-polygons with original snl_ids
buffered_mines <- function(mines, buffersize){
  
  mines_projected <- sf::st_transform(mines, crs = "+proj=robin")
  
  mines_projected_buffer <- sf::st_buffer(mines_projected, dist = buffer) %>%  
    sf::st_union() %>%  
    sf::st_cast("POLYGON")  %>% 
    sf::st_sf() %>% 
    dplyr::mutate(id_buffer = 1:n())
  
  mines_projected_buffer <- st_join(mines_projected_buffer,mines_projected, join = st_intersects)
  
  # Unproject mines
  mines_unprojected_buffer <- sf::st_transform(mines_projected_buffer, crs = "+proj=longlat")
  sf::st_write(mines_unprojected_buffer, ("./output/buffered_mines.shp"))
}

# Call function
buffered_mines(mines, buffer)


#######################################################################
# Calculation of deforestation area by year for a sample dataset of mines

# Read mine locations and deforestation layer, set buffer size (here 20 km)
mines <- sf::st_read("./input/snl_samples.shp")
deforestation_year <- raster::raster("./input/Hansen_GFC-2017-v1.5_lossyear_00N_060W.tif")
buffer <- units::set_units(20, km)

# Return unprojected buffered mine-polygons with original snl_ids
buffered_mines(mines, buffer)

# Calculating area of raster pixel of deforestation file
pixel_area <- raster::area(deforestation_year)
deforestation_area <- raster::stack(list(year = deforestation_year, area = pixel_area))
deforestation_area

# Extract values from raster
buffered_mines <- buffered_mines %>% 
  dplyr::mutate(raster_values = raster::extract(deforestation_area, as(buffered_mines, "Spatial")))

# calculate area using pixel area
buffered_mines <- buffered_mines %>% 
  dplyr::mutate(raster_values = lapply(raster_values, tibble::as_tibble)) %>% 
  tidyr::unnest() %>% 
  dplyr::filter(year > 0) %>% 
  dplyr::group_by(id_buffer, year) %>% 
  dplyr::summarise(n_px = n(), area = sum(area, na.rm = TRUE)) 

# Aggregate deforestation by year
mines_deforestation_year <- buffered_mines %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(area = sum(area, na.rm = TRUE)) 

# Save data to file
sf::st_write(mines_deforestation_year, ("./output/snl_samples_deforestation.shp"))





 

