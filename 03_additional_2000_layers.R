library(tidyverse)
library(gdalUtils)
library(raster)
library(snow)
source("./R/resample_gdal.R")
source("./R/parllel_land_mask.R")
source("./R/proxymity_gdal.R")

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
src_land_mask <- paste0(fineprint_grid_30sec_path, "/template_land_mask_30sec.tif")
src_esa_cci_2000 <- paste0(data_path, "/rstudio/esa-cci/land-cover/2000/v2-0-7/esacci_lc_2000_v2-0-7.tif")

# --------------------------------------------------------------------------------------
# destination data files 
dst_cropland_mask_2000 <- paste0(fineprint_grid_30sec_path, "/mask_cropland_2000.tif")
dst_distance_cropland_2000 <- paste0(fineprint_grid_30sec_path, "/distance_cropland_2000.tif")

# --------------------------------------------------------------------------------------
# get 30sec grid land mask template 
land_mask_grid_30sec <- raster::stack(src_land_mask)

# --------------------------------------------------------------------------------------
# calculate distance to urban in 2000 
# if(!file.exists(dst_distance_cropland_2000)){

  # resample lc map 
  f_tmp_p <- stringr::str_glue("{raster::rasterTmpFile()}.vrt")
  system.time(
    gdalUtils::gdalwarp(srcfile = src_esa_cci_2000,
                        dstfile = f_tmp_p,
                        r = "mode",
                        multi = TRUE,
                        co = c("COMPRESS=LZW", "NUM_THREADS=ALL_CPUS"),
                        te = as.vector(raster::extent(land_mask_grid_30sec))[c(1,3,2,4)], 
                        ts = c(ncol(land_mask_grid_30sec), nrow(land_mask_grid_30sec)), 
                        t_srs = "+proj=longlat", 
                        verbose = TRUE,
                        of = "VRT",
                        overwrite = TRUE)
  )  
  
  # create cropland mask 
  in_lc_calc_aggregate <- '1*((A>=10)*(A<=30))'
  make_mapped_mask <- stringr::str_glue("gdal_calc.py \\
  -A {f_tmp_p} \\
  --calc=\'{in_lc_calc_aggregate}\' \\
  --outfile={dst_cropland_mask_2000} \\
  --overwrite --type=Byte --NoDataValue=0 --co COMPRESS=LZW --co NUM_THREADS=ALL_CPUS")
  system.time(system(make_mapped_mask))

  # --------------------------------------------------------------------------------------
  # project to Equidistant Cylindrical (Plate Carrée)
  f_tmp_p <- stringr::str_glue("{raster::rasterTmpFile()}")
  system.time(gdalUtils::gdalwarp(srcfile = dst_cropland_mask_2000, r = "max",
                                  dstfile = f_tmp_p, t_srs = "+proj=eqc", 
                                  co = list("compress=LZW", "TILED=YES", "NUM_THREADS=ALL_CPUS"), 
                                  verbose = TRUE, overwrite = TRUE))
  
  # calculate distance map in meters 
  f_tmp_d <- stringr::str_glue("{raster::rasterTmpFile()}")
  system.time(
        system(paste0("gdal_proximity.py ", f_tmp_p," ", f_tmp_d,
                      " -values 1 -ot Float32 -co compress=LZW -co TILED=YES -co NUM_THREADS=ALL_CPUS -distunits GEO -nodata NA")))

  
  # resample raster back to longlat 
  f_tmp_l <- stringr::str_glue("{raster::rasterTmpFile()}.vrt")
  system.time(gdalUtils::gdalwarp(srcfile = f_tmp_d, r = "bilinear", 
                                  dstfile = f_tmp_l, 
                                  te = as.vector(raster::extent(land_mask_grid_30sec))[c(1,3,2,4)], 
                                  ts = c(ncol(land_mask_grid_30sec), nrow(land_mask_grid_30sec)), 
                                  t_srs = "+proj=longlat", 
                                  co = list("compress=LZW", "TILED=YES", "NUM_THREADS=ALL_CPUS"), 
                                  verbose = TRUE, overwrite = TRUE))

  # harmonize no-land value to NA
  raster::beginCluster()
  parllel_land_mask(x = raster::stack(list(f_tmp_l, land_mask_grid_30sec)),
                    dst_file = dst_distance_cropland_2000, co = c("COMPRESS=LZW", "TILED=YES"), 
                    overwrite = TRUE, verbose = TRUE)
  raster::endCluster()
  
# }

