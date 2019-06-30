library(tidyverse)
library(gdalUtils)
library(raster)
library(magrittr)
library(sf)
library(parallel)
library(fasterize)
source("./R/resample_gdal.R")
source("./R/parllel_land_mask.R")
source("./R/proxymity_gdal.R")

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
src_esa_cci_2000 <- paste0(data_path, "/rstudio/esa-cci/land-cover/2000/v2-0-7/esacci_lc_2000_v2-0-7.tif")
src_mines <- paste0(data_path, "/mine_polygons/mine_polygons_v1r3.geojson")

# --------------------------------------------------------------------------------------
# destination data files 
dst_urban_mask_2000 <- paste0(fineprint_grid_30sec_path, "/urban_mask_2000.tif")
dst_mine_mask_2000 <- paste0(fineprint_grid_30sec_path, "/mine_mask_2000.tif")
dst_distance_urban_2000 <- paste0(fineprint_grid_30sec_path, "/distance_urban_2000.tif")
dst_distance_mine_2000 <- paste0(fineprint_grid_30sec_path, "/distance_mine_2000.tif")

# --------------------------------------------------------------------------------------
# get 30sec grid land mask template 
land_mask_grid_30sec <- raster::stack(src_land_mask)

# --------------------------------------------------------------------------------------
# build 30sec grid mask of urban and mine for 2000 
if(!file.exists(dst_urban_mask_2000) | !file.exists(dst_mine_mask_2000)){
  
  # create mine mask 
  mine_mask <- raster::rasterTmpFile()
  system.time(
    gdalUtils::gdal_rasterize(src_datasource = src_mines, burn = 1, at = TRUE, a_nodata = NA,
                              dst_filename = mine_mask, co = list("compress=LZW", "TILED=YES"),
                              te = as.vector(raster::extent(raster::raster(src_esa_cci_2000)))[c(1,3,2,4)], 
                              ts = c(ncol(raster::raster(src_esa_cci_2000)), nrow(raster::raster(src_esa_cci_2000))), 
                              verbose = TRUE))
  
  # create urban mask 2000
  urban_mask <- raster::rasterTmpFile()
  system.time(raster::clusterR(x = raster::raster(src_esa_cci_2000), fun = raster::overlay, args = list(fun = function(x) ifelse(x == 190, 1, NA)), 
                               filename = urban_mask, options = c("compress=LZW", "TILED=YES"), overwrite = TRUE, verbose = TRUE))
  
  # make mine urban mask for 2000 
  mine_urban_mask_2000 <- raster::rasterTmpFile()
  system.time(raster::clusterR(x = raster::stack(c(x = urban_mask, m = mine_mask)), 
                               fun = raster::overlay, args = list(fun = function(x, m) ifelse(is.na(x), x, as.numeric(is.na(m)))), 
                               filename = mine_urban_mask_2000, options = c("compress=LZW", "TILED=YES"), overwrite = TRUE, verbose = TRUE))
  
  # resample mask mine urban 2000
  tmp_mask_30sec <- raster::rasterTmpFile()
  resample_gdal(src_file = mine_urban_mask_2000,
                dst_file = tmp_mask_30sec, 
                land_mask = src_land_mask, 
                r = "mode", co = list("compress=LZW", "TILED=YES"),
                overwrite = TRUE, verbose = TRUE) 
  
  # write mine mask 2000
  system.time(raster::clusterR(x = tmp_mask_30sec, fun = raster::overlay, args = list(fun = function(x) ifelse(x == 0, 1, NA)), 
                               filename = dst_mine_mask_2000, options = c("compress=LZW", "TILED=YES"), overwrite = TRUE, verbose = TRUE))
  
  # write urban mask 2000
  system.time(raster::clusterR(x = tmp_mask_30sec, fun = raster::overlay, args = list(fun = function(x) ifelse(x == 1, 1, NA)), 
                               filename = dst_urban_mask_2000, options = c("compress=LZW", "TILED=YES"), overwrite = TRUE, verbose = TRUE))
  
}

# --------------------------------------------------------------------------------------
# calculate distance to mine area in 2000 
if(!file.exists(dst_distance_mine_2000)){
  
  # project to Equidistant Cylindrical (Plate Carrée)
  f_tmp_p <- raster::rasterTmpFile()
  system.time(gdalUtils::gdalwarp(srcfile = dst_mine_mask_2000, r = "near", 
                                  dstfile = f_tmp_p, t_srs = "+proj=eqc", co = list("compress=LZW", "TILED=YES"), 
                                  verbose = TRUE, overwrite = TRUE))
  
  # calculate distance map in meters 
  f_tmp_d <- raster::rasterTmpFile()
  system.time(
        system(paste0("gdal_proximity.py ", f_tmp_p," ", f_tmp_d,
                      " -values 1 -ot Float32 -co compress=LZW -co TILED=YES -distunits GEO -nodata NA")))

  
  # resample raster back to longlat 
  f_tmp_l <- raster::rasterTmpFile()
  system.time(gdalUtils::gdalwarp(srcfile = f_tmp_d, r = "bilinear", 
                                  dstfile = f_tmp_l, 
                                  te = as.vector(raster::extent(land_mask_grid_30sec))[c(1,3,2,4)], 
                                  ts = c(ncol(land_mask_grid_30sec), nrow(land_mask_grid_30sec)), 
                                  t_srs = "+proj=longlat", 
                                  co = list("compress=LZW", "TILED=YES"), 
                                  verbose = TRUE, overwrite = TRUE))
  
  # harmonize no-land value to NA
  parllel_land_mask(x = raster::stack(list(f_tmp_l, land_mask_grid_30sec)),
                    dst_file = dst_distance_mine_2000, co = c("COMPRESS=LZW", "TILED=YES"), 
                    overwrite = TRUE, verbose = TRUE)
  
}

# --------------------------------------------------------------------------------------
# calculate distance to mine area in 2000 
if(!file.exists(dst_distance_urban_2000)){
  
  # project to Equidistant Cylindrical (Plate Carrée)
  f_tmp_p <- raster::rasterTmpFile()
  system.time(gdalUtils::gdalwarp(srcfile = dst_urban_mask_2000, r = "near", 
                                  dstfile = f_tmp_p, t_srs = "+proj=eqc", co = list("compress=LZW", "TILED=YES"), 
                                  verbose = TRUE, overwrite = TRUE))
  
  # calculate distance map in meters 
  f_tmp_d <- raster::rasterTmpFile()
  system.time(
    system(paste0("gdal_proximity.py ", f_tmp_p," ", f_tmp_d,
                  " -values 1 -ot Float32 -co compress=LZW -co TILED=YES -distunits GEO -nodata NA")))
  
  # resample raster back to longlat 
  f_tmp_l <- raster::rasterTmpFile()
  system.time(gdalUtils::gdalwarp(srcfile = f_tmp_d, r = "bilinear", 
                                  dstfile = f_tmp_l, 
                                  te = as.vector(raster::extent(land_mask_grid_30sec))[c(1,3,2,4)], 
                                  ts = c(ncol(land_mask_grid_30sec), nrow(land_mask_grid_30sec)), 
                                  t_srs = "+proj=longlat", 
                                  co = list("compress=LZW", "TILED=YES"), 
                                  verbose = TRUE, overwrite = TRUE))
  
  # harmonize no-land value to NA
  parllel_land_mask(x = raster::stack(list(f_tmp_l, land_mask_grid_30sec)),
                    dst_file = dst_distance_urban_2000, co = c("COMPRESS=LZW", "TILED=YES"), 
                    overwrite = TRUE, verbose = TRUE)
  
}




