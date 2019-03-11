# Rasterize biomes 

library(fineprintutils)
library(raster)
library(gdalUtils)
library(tidyverse)
library(sf)
library(fasterize)
library(parallel)
rasterOptions()

# Get echo regions 
fineprintutils::download_file(url = "https://storage.googleapis.com/teow2016/Ecoregions2017.zip", destfile = "./input/ecoregions/Ecoregions2017.zip", mode = "wb")
unzip(zipfile = "./input/ecoregions/Ecoregions2017.zip", exdir = "./input/ecoregions/")
ecoregions_sf <- sf::read_sf("./input/ecoregions/Ecoregions2017.shp")

# # Get protected areas 
# fineprintutils::download_file(url = "https://protectedplanet.net/downloads/WDPA_Feb2019?type=shapefile", destfile = "./input/protected_areas/WDPA_Feb2019.zip", mode = "wb")
# unzip(zipfile = "./input/protected_areas/WDPA_Feb2019.zip", exdir = "./input/protected_areas/")
protected_areas_sf <- sf::read_sf("./input/protected_areas/WDPA_Feb2019-shapefile-polygons.shp")

# Raster for testing 
deforestation_r <- raster::raster("./input/hansen/Hansen_GFC-2017-v1.5_lossyear_00N_060W.tif")

rasterize_to_landsat_tile <- function(raster_path, sf_obj, field, filepath, background = NA, fun = 'last', by = NULL, ...){
  
  dir.create(path = filepath, showWarnings = FALSE, recursive = TRUE)
  
  r_template <- raster::raster(raster::raster(raster_path))
  
  r <- fasterize::fasterize(sf = sf_obj, raster = r_template, background = background, field = field, fun = fun)
  
  fname <- basename("./input/hansen/Hansen_GFC-2017-v1.5_lossyear_00N_060W.tif") %>% 
    stringr::str_replace("Hansen_GFC-2017-v1.5_lossyear_", "") %>% 
    stringr::str_glue(filepath, "/ECO_ID_", .) 
    
  raster::writeRaster(x = r, filename = fname, ...)
  
  return(fname)
  
}

r_path <- rasterize_to_landsat_tile(raster_path = "./input/hansen/Hansen_GFC-2017-v1.5_lossyear_00N_060W.tif",
                                    sf = ecoregions_sf, 
                                    field = "ECO_ID", 
                                    filepath = "./input/ecoregions", 
                                    overwrite = TRUE, progress = 'text', dataType = "INT2U", options = "COMPRESS=LZW")
  

r1 <- raster::raster("input/ecoregions/ECO_ID_00N_060W_no-compress.tif")
r2 <- raster::raster("input/ecoregions/ECO_ID_00N_060W.tif")
plot(r2)

# Rarterize can be done on demand 
# Area if not found to be faster, pre-process all tiles 

