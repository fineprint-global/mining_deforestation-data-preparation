library(tidyverse)
library(sf)
library(raster)
library(units)

# Read mine locations and deforestation layer
mines <- sf::st_read("./input/snl_samples.shp")
deforestation_year <- raster::raster("./input/Hansen_GFC-2017-v1.5_lossyear_00N_060W.tif")
view(mines)
view(deforestation)

# Plot deforestation and mines without reproject
image(deforestation)
plot(mines, add = TRUE, col = "black")

# Reproject vector (and raster/not needed)
mines_projected <- sf::st_transform(mines, crs = "+proj=robin")
#deforestation_year_reproject <- deforestation_year %>% raster::projectRaster(crs = "+proj=robin", method = "ngb")
view(mines_projected)

#######################################################################################
# Plot reprojected data
image(deforestation__year_reproject)
plot(mines_projected, add = TRUE, col = "black")

# Create Buffer Areas of mines (20 km) and merge overlapping areas; trace back to mines
buffer <- units::set_units(20, km)
mines_projected_buffer <- sf::st_buffer(mines_projected, dist = buffer, nQuadSegs = 30, endCapStyle = "ROUND",
                                        joinStyle = "ROUND", mitreLimit = 1) %>%  
  sf::st_union() %>%  
  sf::st_cast("POLYGON")  %>% 
  sf::st_sf() %>% 
  dplyr::mutate(id_buffer = 1:n())

mines_projected_buffer <- st_join(mines_projected_buffer,mines_projected, join = st_intersects)

# Unproject mines
mines_unprojected_buffer <- sf::st_transform(mines_projected_buffer, crs = "+proj=longlat")

# Calculating area of raster pixel of deforestation file
pixel_area <- raster::area(deforestation_year)
deforestation_area <- raster::stack(list(year = deforestation_year, area = pixel_area))
deforestation_area

# Extract values from raster
mines_unprojected_buffer <- mines_unprojected_buffer %>% 
  dplyr::mutate(raster_values = raster::extract(deforestation_area, as(mines_unprojected_buffer, "Spatial")))

# calculate area using pixel area
mines_unprojected_buffer <- mines_unprojected_buffer %>% 
  dplyr::mutate(raster_values = lapply(raster_values, tibble::as_tibble)) %>% 
  tidyr::unnest() %>% 
  dplyr::filter(year > 0) %>% 
  dplyr::group_by(id_buffer, year) %>% 
  dplyr::summarise(n_px = n(), area = sum(area, na.rm = TRUE)) 


# Aggregate deforestation by year
mines_deforestation_year <- mines_unprojected_buffer %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(area = sum(area, na.rm = TRUE)) 

# Save data to file
sf::st_write(mines_deforestation_year, ("./output/snl_samples_deforestation.shp"))

