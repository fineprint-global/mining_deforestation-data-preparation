library(tidyverse)
library(gdalUtils)
library(raster)
library(magrittr)
raster::rasterOptions(tmpdir = "./raster_tmp/")
raster::rasterOptions(progress = "text")

# --------------------------------------------------------------------------------------
# set path to data sets 
if(!exists("data_path"))
  data_path <- "/mnt/nfs_fineprint/data/geoserver"

# --------------------------------------------------------------------------------------
# set output file 
fineprint_grid_30sec_path <- path.expand(paste0(data_path, "/fineprint_grid_30sec"))
dir.create(fineprint_grid_30sec_path, showWarnings = FALSE, recursive = TRUE)

# --------------------------------------------------------------------------------------
# start raster cluster for parallel processing  
raster::beginCluster()

# --------------------------------------------------------------------------------------
# get 30sec grid template from gpw population 
if(!file.exists(paste0(fineprint_grid_30sec_path, "/template_grid_30sec.tif"))){
  path.expand(paste0(data_path, "/rstudio/sedac-2019/population_density/2000/v4-11-05/gpw_v4_population_density_rev11_2000_30_sec.tif")) %>% 
    raster::brick() %>% 
    raster::brick(nl = 1) %>% 
    raster::writeRaster(filename = paste0(fineprint_grid_30sec_path, "/template_grid_30sec.tif"), overwrite = TRUE)  
} 
path_grid_30sec <- paste0(fineprint_grid_30sec_path, "/template_grid_30sec.tif")
grid_30sec <- raster::stack(path_grid_30sec)

# --------------------------------------------------------------------------------------
# compress population density 
system.time(
  gdalUtils::gdal_translate(src_dataset = path.expand(paste0(data_path, "/rstudio/sedac-2019/population_density/2000/v4-11-05/gpw_v4_population_density_rev11_2000_30_sec.tif")),
                            dst_dataset = paste0(fineprint_grid_30sec_path, "/population_density_2000.tif"), 
                            ot = "Float32", co = list("compress=LZW", "TILED=YES"), verbose = TRUE))

# --------------------------------------------------------------------------------------
# resample soilgrid using majority 
system.time(
  gdalUtils::gdalwarp(srcfile = path.expand(paste0(data_path, "/rstudio/soilgrids-2019/soil_classification_250m_taxnwrb/2017/dl_2019-05/soil_classification_250m_TAXNWRB.tif")), 
                      dstfile = "./raster_tmp/soilgrid.tif",r = "mode", ot = "Byte", co = list("compress=LZW", "TILED=YES"),
                      te = as.vector(raster::extent(grid_30sec))[c(1,3,2,4)], 
                      te_srs = raster::projection(grid_30sec), 
                      ts = c(ncol(grid_30sec), nrow(grid_30sec)), 
                      verbose = TRUE, overwrite = TRUE))

# harmonize no-value to NA
system.time(clusterR(x = raster::stack(c("./raster_tmp/soilgrid.tif", paste0(fineprint_grid_30sec_path, "/population_density_2000.tif"))), 
                     fun = raster::overlay, arg = list(fun = function(x, y) ifelse(is.na(y), y, x)), 
                     filename = paste0(fineprint_grid_30sec_path, "/soilgrid.tif"), 
                     options = c("COMPRESS=LZW", "TILED=YES"), datatype = "INT1U", overwrite = TRUE))

# --------------------------------------------------------------------------------------
# resample esa_cci_2000 using majority 
system.time(
  gdalUtils::gdalwarp(srcfile = path.expand(paste0(data_path, "/rstudio/esa-cci/land-cover/2000/v2-0-7/esacci_lc_2000_v2-0-7.tif")), 
                      dstfile = "./raster_tmp/esa_cci_2000.tif",r = "mode", ot = "Byte", co = list("compress=LZW", "TILED=YES"),
                      te = as.vector(raster::extent(grid_30sec))[c(1,3,2,4)], 
                      te_srs = raster::projection(grid_30sec), 
                      ts = c(ncol(grid_30sec), nrow(grid_30sec)), 
                      verbose = TRUE, overwrite = TRUE))

# harmonize no-value to NA
system.time(
  clusterR(x = raster::stack(c("./raster_tmp/esa_cci_2000.tif", paste0(fineprint_grid_30sec_path, "/population_density_2000.tif"))), 
  fun = raster::overlay, arg = list(fun = function(x, y) ifelse(is.na(y), y, x)), 
  filename = paste0(fineprint_grid_30sec_path, "/esa_cci_2000.tif"), 
  options = c("COMPRESS=LZW", "TILED=YES"), datatype = "INT1U", overwrite = TRUE))

# --------------------------------------------------------------------------------------
# resample elevation using bilinear resampling 
system.time(
  gdalUtils::gdalwarp(srcfile = path.expand(paste0(data_path, "/rstudio/amatulli-etal/topographic_variables/2018/dl_2019-07/elevation_1KMmn_GMTEDmn.tif")), 
                      dstfile = "./raster_tmp/elevation.tif",r = "bilinear", ot = "Float32", co = list("compress=LZW", "TILED=YES"),
                      te = as.vector(raster::extent(grid_30sec))[c(1,3,2,4)], 
                      te_srs = raster::projection(grid_30sec), 
                      ts = c(ncol(grid_30sec), nrow(grid_30sec)), 
                      verbose = TRUE, overwrite = TRUE))

# harmonize no-value to NA
system.time(
  clusterR(x = raster::stack(c("./raster_tmp/elevation.tif", paste0(fineprint_grid_30sec_path, "/population_density_2000.tif"))), 
           fun = raster::overlay, arg = list(fun = function(x, y) ifelse(is.na(y), y, x)), 
           filename = paste0(fineprint_grid_30sec_path, "/elevation.tif"), 
           options = c("COMPRESS=LZW", "TILED=YES"), datatype = "FLT4S", overwrite = TRUE))

# --------------------------------------------------------------------------------------
# resample slope using bilinear resampling 
system.time(
  gdalUtils::gdalwarp(srcfile = path.expand(paste0(data_path, "/rstudio/amatulli-etal/topographic_variables/2018/dl_2019-07/slope_1KMmn_GMTEDmn.tif")), 
                      dstfile = "./raster_tmp/slope.tif",r = "bilinear", ot = "Float32", co = list("compress=LZW", "TILED=YES"),
                      te = as.vector(raster::extent(grid_30sec))[c(1,3,2,4)], 
                      te_srs = raster::projection(grid_30sec), 
                      ts = c(ncol(grid_30sec), nrow(grid_30sec)), 
                      verbose = TRUE, overwrite = TRUE))

# harmonize no-value to NA
system.time(
  clusterR(x = raster::stack(c("./raster_tmp/slope.tif", paste0(fineprint_grid_30sec_path, "/population_density_2000.tif"))), 
           fun = raster::overlay, arg = list(fun = function(x, y) ifelse(is.na(y), y, x)), 
           filename = paste0(fineprint_grid_30sec_path, "/slope.tif"), 
           options = c("COMPRESS=LZW", "TILED=YES"), datatype = "FLT4S", overwrite = TRUE))

# --------------------------------------------------------------------------------------
# stop raster cluster 
raster::endCluster()
