library(tidyverse)
library(gdalUtils)
library(raster)
library(magrittr)
library(sf)
library(parallel)

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
if(!file.exists(paste0(fineprint_grid_30sec_path, "/population_density_2000.tif"))){
  system.time(
    gdalUtils::gdal_translate(src_dataset = path.expand(paste0(data_path, "/rstudio/sedac-2019/population_density/2000/v4-11-05/gpw_v4_population_density_rev11_2000_30_sec.tif")),
                              dst_dataset = paste0(fineprint_grid_30sec_path, "/population_density_2000.tif"), 
                              ot = "Float32", co = list("compress=LZW", "TILED=YES"), verbose = TRUE))
}

# --------------------------------------------------------------------------------------
# resample soilgrid using majority 
if(!file.exists(paste0(fineprint_grid_30sec_path, "/soilgrid.tif"))){
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
}

# --------------------------------------------------------------------------------------
# resample esa_cci_2000 using majority 
if(!file.exists(paste0(fineprint_grid_30sec_path, "/esa_cci_2000.tif"))){
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
}
# --------------------------------------------------------------------------------------
# resample elevation using bilinear resampling 
if(!file.exists(paste0(fineprint_grid_30sec_path, "/elevation.tif"))){
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
}

# --------------------------------------------------------------------------------------
# resample slope using bilinear resampling 
if(!file.exists(paste0(fineprint_grid_30sec_path, "/slope.tif"))){
  system.time(
    gdalUtils::gdalwarp(srcfile = path.expand(paste0(data_path, "/rstudio/amatulli-etal/topographic_variables/2018/dl_2019-07/slope_1KMmn_GMTEDmn.tif")), 
                        dstfile = "./raster_tmp/slope.tif", r = "bilinear", ot = "Float32", co = list("compress=LZW", "TILED=YES"),
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
}

# --------------------------------------------------------------------------------------
# create 30sec grid with distance to roads


# subset open street map categories 
waterway_categories <- c("river", "canal")
names(waterway_categories) <- waterway_categories
highway_categories <- c("primary", "motorway", "secondary", "trunk")
names(highway_categories) <- highway_categories

# write to file split waterway data into categories 
waterway_path <- path.expand("./raster_tmp/waterway")
dir.create(path = waterway_path, showWarnings = FALSE, recursive = TRUE)
lapply(waterway_categories, function(f){
  if(!exists("osm_tiles")){
    osm_tiles <- lapply(dir(path.expand(paste0(data_path, "/openstreetmap/infrastructure/2019/dl_2019-05")), pattern = "gpkg$", full.names = TRUE), 
                        FUN = sf::st_read, 
                        query = "SELECT highway, waterway, geom AS geometry FROM lines WHERE highway IS NOT NULL OR waterway IS NOT NULL")
  }
  f_out <- paste0(waterway_path, "/waterway_", f, ".gpkg")
  if(!file.exists(f_out)){
    waterway <- lapply(osm_tiles, dplyr::filter, waterway %in% waterway_categories)
    waterway_sf <- lapply(waterway_categories, FUN = function(f) do.call("rbind", parallel::mclapply(waterway, mc.cores = 5, dplyr::filter, waterway %in% f)))
    waterway_sf[[f]] %>% 
      dplyr::transmute(waterway = 1) %>% 
      sf::st_write(dsn = f_out, delete_dsn = TRUE)
  }
})

# write to file split highway data into categories 
highway_path <- path.expand("./raster_tmp/highway")
dir.create(path = highway_path, showWarnings = FALSE, recursive = TRUE)
lapply(highway_categories, function(f){
  if(!exists("osm_tiles")){
    osm_tiles <- lapply(dir(path.expand(paste0(data_path, "/openstreetmap/infrastructure/2019/dl_2019-05")), pattern = "gpkg$", full.names = TRUE), 
                        FUN = sf::st_read, 
                        query = "SELECT highway, waterway, geom AS geometry FROM lines WHERE highway IS NOT NULL OR waterway IS NOT NULL")
  }
  f_out <- paste0(highway_path, "/highway_", f, ".gpkg")
  highway  <- lapply(osm_tiles, dplyr::filter, highway %in% highway_categories)
  highway_sf <- lapply(highway_categories, FUN = function(f) do.call("rbind", parallel::mclapply(highway, mc.cores = 5, dplyr::filter, highway %in% f)))
  if(!file.exists(f_out)){
    highway_sf[[f]] %>% 
      dplyr::transmute(highway = 1) %>% 
      sf::st_write(dsn = f_out, delete_dsn = TRUE) 
  }
})

# rasterize and build 30sec grid with distance to highway and waterway
f_list <- c(dir(waterway_path, pattern = ".gpkg$", full.names = TRUE), 
            dir(highway_path, pattern = ".gpkg$", full.names = TRUE))

lapply(f_list, function(f_in){
  
  f_tmp_f <- paste0("./raster_tmp/field_", stringr::str_replace_all(basename(f_in), ".gpkg", ".tif"))
  f_tmp_d <- paste0("./raster_tmp/dist_", stringr::str_replace_all(basename(f_in), ".gpkg", ".tif"))
  f_out <- paste0(data_path, "/fineprint_grid_30sec/distance_",  stringr::str_replace_all(basename(f_in), ".gpkg", ".tif"))
  
  if(!file.exists(f_out)){
    
    # rasterize open street map 
    system.time(
      gdalUtils::gdal_rasterize(src_datasource = f_in, burn = 1, at = TRUE, a_nodata = NA,
                                dst_filename = f_tmp_f, ot = "Byte", co = list("compress=LZW", "TILED=YES"),
                                te = as.vector(raster::extent(grid_30sec))[c(1,3,2,4)], 
                                ts = c(ncol(grid_30sec), nrow(grid_30sec)), 
                                verbose = TRUE))
    
    # calculate distance map 
    system.time(
      system(paste0("gdal_proximity.py ", f_tmp_f," ", f_tmp_d, 
                    " -values 1 -ot Float32 -co compress=LZW -co TILED=YES -nodata NA -maxdist 1000000")))
    
    # harmonize no-value to NA
    system.time(
      clusterR(x = raster::stack(c(f_tmp_d, paste0(fineprint_grid_30sec_path, "/population_density_2000.tif"))), 
               fun = raster::overlay, arg = list(fun = function(x, y) ifelse(is.na(y), y, x)), 
               filename = f_out, options = c("COMPRESS=LZW", "TILED=YES"), datatype = "FLT4S", overwrite = TRUE))
    
  }
  
  return(f_out)
  
})

# --------------------------------------------------------------------------------------
# build 30sec grid for mine distance 
f_in <- path.expand(paste0(data_path, "/mine_polygons/mine_polygons_v1r3.geojson")) 
f_out <- path.expand(paste0(fineprint_grid_30sec_path, "/distance_mine.tif")) 
f_tmp_f <- paste0("./raster_tmp/mine_polygons_field.tif")
f_tmp_d <- paste0("./raster_tmp/mine_polygons_dist.tif")

# rasterize open street map 
if(!file.exists(f_out)){
  
  # rasterize open street map 
  system.time(
    gdalUtils::gdal_rasterize(src_datasource = f_in, burn = 1, at = TRUE, a_nodata = NA,
                              dst_filename = f_tmp_f, ot = "Byte", co = list("compress=LZW", "TILED=YES"),
                              te = as.vector(raster::extent(grid_30sec))[c(1,3,2,4)], 
                              ts = c(ncol(grid_30sec), nrow(grid_30sec)), 
                              verbose = TRUE))
  
  # calculate distance map 
  system.time(
    system(paste0("gdal_proximity.py ", f_tmp_f," ", f_tmp_d, 
                  " -values 1 -ot Float32 -co compress=LZW -co TILED=YES -nodata NA -maxdist 1000000")))
  
  # harmonize no-value to NA
  system.time(
    clusterR(x = raster::stack(c(f_tmp_d, paste0(fineprint_grid_30sec_path, "/population_density_2000.tif"))), 
             fun = raster::overlay, arg = list(fun = function(x, y) ifelse(is.na(y), y, x)), 
             filename = f_out, options = c("COMPRESS=LZW", "TILED=YES"), datatype = "FLT4S", overwrite = TRUE))
  
}

# path.expand(paste0(data_path, "/protected_areas/WDPA_Feb2019-shapefile-polygons.shp"))
# distance_protected_area <- path.expand(paste0(fineprint_grid_30sec_path, "/distance_protected_areas.tif"))

# --------------------------------------------------------------------------------------
# stop raster cluster 
raster::endCluster()

sapply(dir("./raster_tmp", full.names = TRUE), unlink, recursive = FALSE)

