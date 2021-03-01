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
# get 30sec land mask template 
land_mask_land_mask_grid_30sec <- raster::stack(paste0(fineprint_grid_30sec_path, "/template_land_mask_30sec.tif"))

# --------------------------------------------------------------------------------------
# source data files 
src_osm_dir <- paste0(data_path, "/openstreetmap/infrastructure/2019/dl_2019-05")

# --------------------------------------------------------------------------------------
# destination data files 
dst_osm_dir <- fineprint_grid_30sec_path

# --------------------------------------------------------------------------------------
# define open street maps subsets 
osm_categories <- c(waterway_river = "river", waterway_canal = "canal", 
                    highway_primary = "primary", highway_motorway = "motorway", 
                    highway_secondary = "secondary", highway_trunk = "trunk")

# --------------------------------------------------------------------------------------
# split waterway and highway data files into categories 
tmp_osm_path <- paste0(raster::rasterOptions()$tmpdir, "tmp_osm")
dir.create(path = tmp_osm_path, showWarnings = FALSE, recursive = TRUE)

if(!exists("osm_tiles")){
  
  osm_flist <- c(
    "/mnt/nfs_fineprint/data/geoserver/openstreetmap/infrastructure/2019/dl_2019-05/africa.gpkg",
    "/mnt/nfs_fineprint/data/geoserver/openstreetmap/infrastructure/2019/dl_2019-05/asia.gpkg",
    "/mnt/nfs_fineprint/data/geoserver/openstreetmap/infrastructure/2019/dl_2019-05/australia-oceania.gpkg",
    "/mnt/nfs_fineprint/data/geoserver/openstreetmap/infrastructure/2019/dl_2019-05/central-america.gpkg",
    "/mnt/nfs_fineprint/data/geoserver/openstreetmap/infrastructure/2019/dl_2019-11/europe.gpkg",
    "/mnt/nfs_fineprint/data/geoserver/openstreetmap/infrastructure/2019/dl_2019-11/north-america.gpkg",
    "/mnt/nfs_fineprint/data/geoserver/openstreetmap/infrastructure/2019/dl_2019-05/south-america.gpkg"
  )
  
  osm_tiles <- lapply(osm_flist, FUN = sf::st_read, 
                      query = "SELECT highway, waterway, geom AS geometry FROM lines WHERE highway IS NOT NULL OR waterway IS NOT NULL")
}

lapply(names(osm_categories), function(f){
  
  f_out <- paste0(tmp_osm_path, "/", f, ".gpkg")
  
  if(!file.exists(f_out)){
    if(stringr::str_detect(f, pattern = "highway")){
      # osm_sf <- do.call("rbind", parallel::mclapply(osm_tiles, mc.cores = length(osm_tiles), dplyr::filter, highway %in% osm_categories[f]))
      osm_sf <- data.table::rbindlist(parallel::mclapply(osm_tiles, mc.cores = length(osm_tiles), dplyr::filter, highway %in% osm_categories[f]))
    }
    if(stringr::str_detect(f, pattern = "waterway")){
      # osm_sf <- do.call("rbind", parallel::mclapply(osm_tiles, mc.cores = length(osm_tiles), dplyr::filter, waterway %in% osm_categories[f]))
      # osm_sf <- data.table::rbindlist(parallel::mclapply(osm_tiles, mc.cores = length(osm_tiles), dplyr::filter, waterway %in% osm_categories[f]))
      osm_sf <- data.table::rbindlist(parallel::mclapply(osm_tiles, mc.cores = 4, dplyr::filter, waterway %in% osm_categories[f]))
    }
    osm_sf %>% 
      sf::st_as_sf() %>% 
      dplyr::transmute(highway = 1) %>% 
      sf::st_write(dsn = f_out, delete_dsn = TRUE) 
  }
  
})

# rasterize and build 30sec grid with distance to highway and waterway
f_list <- dir(tmp_osm_path, pattern = ".gpkg$", full.names = TRUE)

lapply(f_list, function(f_in){
  
  f_tmp_f <- paste0(tmp_osm_path, "/field_", stringr::str_replace_all(basename(f_in), ".gpkg", ".tif"))
  f_tmp_p <- paste0(tmp_osm_path, "/eqc_", stringr::str_replace_all(basename(f_in), ".gpkg", ".tif"))
  f_tmp_d <- paste0(tmp_osm_path, "/dist_", stringr::str_replace_all(basename(f_in), ".gpkg", ".tif"))
  f_tmp_l <- paste0(tmp_osm_path, "/longlat_", stringr::str_replace_all(basename(f_in), ".gpkg", ".tif"))
  f_out <- paste0(dst_osm_dir, "/distance_",  stringr::str_replace_all(basename(f_in), ".gpkg", ".tif"))
  
  # if(!file.exists(f_out)){
    
    # rasterize open street map 
    # if(!file.exists(f_tmp_f)){
      
      system.time(
        gdalUtils::gdal_rasterize(src_datasource = f_in, burn = 1, at = TRUE, a_nodata = NA,
                                  dst_filename = f_tmp_f, co = list("compress=LZW", "TILED=YES"),
                                  te = as.vector(raster::extent(land_mask_land_mask_grid_30sec))[c(1,3,2,4)], 
                                  ts = c(ncol(land_mask_land_mask_grid_30sec), nrow(land_mask_land_mask_grid_30sec)), 
                                  verbose = TRUE))
      
    # }
    
    # project open street map to Equidistant Cylindrical (Plate Carrée)
    if(!file.exists(f_tmp_p)){
      system.time(gdalUtils::gdalwarp(srcfile = f_tmp_f, r = "near", 
                                      dstfile = f_tmp_p, t_srs = "+proj=eqc", co = list("compress=LZW", "TILED=YES"), 
                                      verbose = TRUE, overwrite = TRUE))
    }
    
    # calculate distance map in meters 
    # if(!file.exists(f_tmp_d)){
      system.time(
        system(paste0("gdal_proximity.py ", f_tmp_p," ", f_tmp_d,
                      " -values 1 -ot Float32 -co compress=LZW -co TILED=YES -distunits GEO -nodata NA")))
    # }
    
    # resample raster back to longlat 
    # if(!file.exists(f_tmp_l)){
      system.time(gdalUtils::gdalwarp(srcfile = f_tmp_d, r = "bilinear", 
                                      dstfile = f_tmp_l, 
                                      te = as.vector(raster::extent(land_mask_land_mask_grid_30sec))[c(1,3,2,4)], 
                                      ts = c(ncol(land_mask_land_mask_grid_30sec), nrow(land_mask_land_mask_grid_30sec)), 
                                      t_srs = "+proj=longlat", 
                                      co = list("compress=LZW", "TILED=YES"), 
                                      verbose = TRUE, overwrite = TRUE))
    # }
    
    # harmonize no-land value to NA
    parllel_land_mask(x = raster::stack(list(f_tmp_l, land_mask_land_mask_grid_30sec)),
                      dst_file = f_out, co = c("COMPRESS=LZW", "TILED=YES"), 
                      overwrite = TRUE, verbose = TRUE)
    
  # }
  
  return(f_out)
  
})

# --------------------------------------------------------------------------------------
# clean tmp folder 
raster::endCluster()

