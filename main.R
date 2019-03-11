library(tidyverse)
library(sf)
library(raster)
library(units)

# Set buffersize in km
buffer <- units::set_units(100, km)

# Get snl-data from geoserver
fineprint_geoserver <- "http://fineprint:(allourdata)@fineprint.wu.ac.at:8080/geoserver/"
snl_wfs_request <- "snl-2018/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=snl-2018:snl_metals&outputFormat=application/json"
snl_mines_wfs <- stringr::str_glue(fineprint_geoserver, snl_wfs_request)
snl_mines <- sf::st_read(snl_mines_wfs) %>% 
  sf::st_transform(crs = sf::st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Function, which  returns unprojected buffered mine-polygons
buffered_mines <- function(mines, buffersize){
  
  mines_projected <- sf::st_transform(mines, crs = "+proj=igh")
  
  mines_projected_buffer <- sf::st_buffer(mines_projected, dist = buffer) %>%  
    sf::st_union() %>%  
    sf::st_cast("POLYGON")  %>% 
    sf::st_sf() %>% 
    dplyr::mutate(id_buffer = 1:n())
  
  mines_unprojected_buffer <- sf::st_transform(mines_projected_buffer, crs = "+proj=longlat")
  mines_unprojected_buffer <- mines_unprojected_buffer %>%
    dplyr::select(id_buffer, geometry)
  
  sf::st_write(mines_unprojected_buffer, ("./output/buffered_mines.shp"))
}

# Call function
buffered_mines(mines, buffer)


# intersection buffered mines and snl: mines_projected_buffer <- st_join(mines_projected_buffer,mines_projected, join = st_intersects)

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





 

