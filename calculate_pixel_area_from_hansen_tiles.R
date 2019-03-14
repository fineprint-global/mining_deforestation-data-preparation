library(tidyverse)
library(raster)
library(sf)
library(fasterize)
library(velox)
library(fineprintutils)

# list Hansen dataset
hansen_tiles <- dir("/mnt/nfs_fineprint/tmp/hansen/lossyear", pattern = ".tif$", full.name = TRUE) 
ecoregions_sf <- sf::read_sf("./input/ecoregions/Ecoregions2017.shp")
area_dir <- "/mnt/nfs_fineprint/tmp/hansen_pixel_area"
dir.create(area_dir, showWarnings = FALSE, recursive = TRUE)

r_area_list <- parallel::mclapply(hansen_tiles, mc.cores = 4, function(f){
  
  output_filename <- paste0(area_dir, "/", basename(f))
  
  if(file.exists(output_filename)){
    return(output_filename)
  }
  
  print(cat("Read raster ", f))
  r <- raster::raster(f)
  
  r_intersects <- sf::st_bbox(r) %>% 
    sf::st_as_sfc() %>% 
    sf::st_intersects(ecoregions_sf, sparse = FALSE) %>% 
    any()
  
  if(!r_intersects){
    return(NULL)
  }
  
  print(cat("Calculate raster area ", f))
  ra <- raster::area(r, na.rm = FALSE, filename = output_filename, overwrite = TRUE)
  
  print(cat("Done ", f))
  return(output_filename)
  
})

  
