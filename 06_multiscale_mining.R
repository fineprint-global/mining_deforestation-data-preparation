library(tidyverse)
library(gdalUtils)
library(raster)

# --------------------------------------------------------------------------------------
# raster package setting 
raster::rasterOptions(tmpdir = "./raster_tmp/")
raster::rasterOptions(format = "GTiff")
raster::rasterOptions(progress = "text")

# --------------------------------------------------------------------------------------
# set path to data sets 
if(!exists("data_path"))
  data_path <- path.expand("/mnt/nfs_fineprint/data/geoserver")

# --------------------------------------------------------------------------------------
# set output file 
fineprint_grid_30sec_path <- path.expand(paste0(data_path, "/fineprint_grid_30sec"))

# --------------------------------------------------------------------------------------
# source data files 
src_mining_area <- paste0(data_path, "/fineprint_grid_30sec/global_miningarea_sqkm_30arcsecond_v1.tif")

# --------------------------------------------------------------------------------------
# define grid aggrenation levels 
grid_levels <- tibble::tribble(        ~name, ~fact,
                                       "5arcminute",       10, # aggregation factor from 30 arcsecond to 5 arcminute (approximately 10 kilometer)
                                       "30arcminute",      60, # aggregation factor from 30 arcsecond to 30 arcminute (approximately 55 kilometer)
                                       "1degree",         120, # aggregation factor from 30 arcsecond to 1 degree (approximately 111 kilometer)
                                       "10degrees",      1200) # aggregation factor from 30 arcsecond to 10 degrees 

r_src <- raster::raster(src_mining_area)
fit_bbox <- raster::extent(r_src)[c(1,3,2,4)]
for(g in 1:nrow(grid_levels) ){
  
  fact <- grid_levels$fact[g]
  grid_name <- grid_levels$name[g]
  fname_area <- stringr::str_glue("{fineprint_grid_30sec_path}/{stringr::str_replace(basename(src_mining_area), '30arcsecond', grid_name)}")

  print(paste("Aggregation factor", fact))
  print(paste("Aggregate grid cell area to", grid_name, "and write to", fname_area))
  f_tmp <- stringr::str_glue("{raster::rasterTmpFile()}.vrt")
  gdal_cmd <- stringr::str_glue("gdalwarp -overwrite {src_mining_area} {f_tmp} -r \'sum\' -te {paste(fit_bbox, collapse = ' ')} -ts {ncol(r_src)/fact} {nrow(r_src)/fact} -of \'VRT\'")
  system.time(system(gdal_cmd))
  
  gdal_cmd <- stringr::str_glue("gdalwarp -overwrite {f_tmp} {fname_area} -r \'mode\' -te {paste(fit_bbox, collapse = ' ')} -ts {ncol(r_src)} {nrow(r_src)}")
  system.time(system(gdal_cmd))
  
}


