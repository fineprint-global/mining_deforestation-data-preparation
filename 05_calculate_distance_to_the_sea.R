library(tidyverse)
library(gdalUtils)
library(raster)
library(magrittr)
library(sf)
library(parallel)
library(fasterize)
source("./R/parllel_land_mask.R")

# --------------------------------------------------------------------------------------
# raster package setting 
raster::rasterOptions(tmpdir = "./raster_tmp/")
raster::rasterOptions(format = "GTiff")
raster::rasterOptions(progress = "text")
raster::beginCluster()

# --------------------------------------------------------------------------------------
# set path to data sets 
if(!exists("data_path"))
  data_path <- path.expand("/mnt/nfs_fineprint/data/geoserver")

# --------------------------------------------------------------------------------------
# set output file 
fineprint_grid_30sec_path <- path.expand(paste0(data_path, "/fineprint_grid_30sec"))

# --------------------------------------------------------------------------------------
# source data files 
src_land_mask <- paste0(fineprint_grid_30sec_path, "/template_land_mask_30sec.tif")

# --------------------------------------------------------------------------------------
# destination data files 
dst_distance_to_sea <- paste0(fineprint_grid_30sec_path, "/distance_to_sea.tif")

# --------------------------------------------------------------------------------------
# build 30sec grid with the distance to the sea 
if(!file.exists(dst_distance_to_sea)){
  
  # create sea mask 
  tmp_sea_mask <- raster::rasterTmpFile()
  system.time(raster::clusterR(x = raster::raster(src_land_mask), fun = raster::overlay, args = list(fun = function(x) ifelse(is.na(x), 1, NA)), 
                               filename = tmp_sea_mask, options = c("compress=LZW", "TILED=YES"), overwrite = TRUE, verbose = TRUE))
  
  # project to Equidistant Cylindrical (Plate Carrée)
  f_tmp_p <- raster::rasterTmpFile()
  system.time(gdalUtils::gdalwarp(srcfile = tmp_sea_mask, r = "near", 
                                  dstfile = f_tmp_p, t_srs = "+proj=eqc", co = list("compress=LZW", "TILED=YES"), 
                                  verbose = TRUE, overwrite = TRUE))
  
  # calculate distance map in meters 
  f_tmp_d <- raster::rasterTmpFile()
  system.time(
    system(paste0("gdal_proximity.py ", f_tmp_p," ", f_tmp_d,
                  " -values 1 -ot Float32 -co compress=LZW -co TILED=YES -distunits GEO -nodata NA")))
  
  # resample raster back to longlat 
  system.time(gdalUtils::gdalwarp(srcfile = f_tmp_d, r = "bilinear", 
                                  dstfile = dst_distance_to_sea, 
                                  te = as.vector(raster::extent(raster::raster(src_land_mask)))[c(1,3,2,4)], 
                                  ts = c(ncol(raster::raster(src_land_mask)), nrow(raster::raster(src_land_mask))), 
                                  t_srs = "+proj=longlat", 
                                  co = list("compress=LZW", "TILED=YES"), 
                                  verbose = TRUE, overwrite = TRUE))
  
  # harmonize no-land value to NA
  f_tmp_l <- raster::rasterTmpFile()
  system.time(parllel_land_mask(x = raster::stack(list(f_tmp_l, src_land_mask)),
                                dst_file = dst_distance_to_sea, co = c("COMPRESS=LZW", "TILED=YES"), 
                                overwrite = TRUE, verbose = TRUE))
  
}

# --------------------------------------------------------------------------------------
# end cluster 
raster::endCluster()


