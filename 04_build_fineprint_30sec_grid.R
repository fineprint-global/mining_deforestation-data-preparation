library(tidyverse)
library(magrittr)
library(raster)
library(velox)
library(fasterize)
library(sf)
library(stars)
library(exactextractr)
library(parallel)
source("./R/build_forest_30sec_grid.R")
raster::rasterOptions(tmpdir = "./raster_tmp/")
raster::rasterOptions(format = "GTiff")



# --------------------------------------------------------------------------------------
# set path to data sets 
if(!exists("data_path"))
  data_path <- "/mnt/nfs_fineprint/data/geoserver"

# --------------------------------------------------------------------------------------
# Forest cover threshold
# 10% forest cover 2000 threshold - https://developers.google.com/earth-engine/tutorials/community/forest-cover-loss-estimation
# 25% forest cover 2000 threshold - https://www.pnas.org/content/107/19/8650
forest_cover_threshold <- 10 # percentage 

# --------------------------------------------------------------------------------------
# set output file 
fineprint_grid_30sec_path <- path.expand(paste0(data_path, "/fineprint_grid_30sec"))
dir.create(fineprint_grid_30sec_path, showWarnings = FALSE, recursive = TRUE)
#output_path <- paste0(fineprint_grid_30sec_path, "/timeseries", format(Sys.time(), "_%Y%m%d%H%M%S"))
output_path <- paste0(fineprint_grid_30sec_path, "/grid_", "_20201209_1")
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

# --------------------------------------------------------------------------------------
# path to mine polygons 
mine_polygons <- path.expand(paste0(data_path, "/mine_polygons/mine_polygons_world_v1r3.geojson"))
country_codes <- path.expand(paste0(fineprint_grid_30sec_path, "/countries_concordance.csv"))

# --------------------------------------------------------------------------------------
# path to 30sec grid files 
grid_file_paths <- 
  c(elevation                  = path.expand(paste0(fineprint_grid_30sec_path, "/elevation.tif")),
    slope                      = path.expand(paste0(fineprint_grid_30sec_path, "/slope.tif")),
    soilgrid                   = path.expand(paste0(fineprint_grid_30sec_path, "/soilgrid.tif")),
    esa_cci_2000               = path.expand(paste0(fineprint_grid_30sec_path, "/esa_cci_2000.tif")),
    pop_2000                   = path.expand(paste0(fineprint_grid_30sec_path, "/population_density_2000.tif")),
    distance_waterway_canal    = path.expand(paste0(fineprint_grid_30sec_path, "/distance_waterway_canal.tif")),
    distance_waterway_river    = path.expand(paste0(fineprint_grid_30sec_path, "/distance_waterway_river.tif")),
    distance_highway_primary   = path.expand(paste0(fineprint_grid_30sec_path, "/distance_highway_primary.tif")),
    distance_highway_motorway  = path.expand(paste0(fineprint_grid_30sec_path, "/distance_highway_motorway.tif")),
    distance_highway_secondary = path.expand(paste0(fineprint_grid_30sec_path, "/distance_highway_secondary.tif")),
    distance_highway_trunk     = path.expand(paste0(fineprint_grid_30sec_path, "/distance_highway_trunk.tif")), 
    distance_mine              = path.expand(paste0(fineprint_grid_30sec_path, "/distance_mine.tif")),
    distance_protected_area    = path.expand(paste0(fineprint_grid_30sec_path, "/distance_protected_areas.tif")),
    distance_sea               = path.expand(paste0(fineprint_grid_30sec_path, "/distance_to_sea.tif")),
    min_area_5arcmin           = path.expand(paste0(fineprint_grid_30sec_path, "/global_miningarea_sqkm_5arcminute_v1.tif")),
    min_area_30arcmin          = path.expand(paste0(fineprint_grid_30sec_path, "/global_miningarea_sqkm_30arcminute_v1.tif")),
    min_area_1degree           = path.expand(paste0(fineprint_grid_30sec_path, "/global_miningarea_sqkm_1degree_v1.tif")),
    min_area_10degree          = path.expand(paste0(fineprint_grid_30sec_path, "/global_miningarea_sqkm_10degrees_v1.tif")),
    contiguous_land            = path.expand(paste0(fineprint_grid_30sec_path, "/contiguous_land.tif")),
    distance_cropland_2000     = path.expand(paste0(fineprint_grid_30sec_path, "/distance_cropland_2000.tif")),
    ecoregions                 = path.expand(paste0(fineprint_grid_30sec_path, "/ecoregions_2017.tif")),
    # distance_urban_2000        = paste0(fineprint_grid_30sec_path, "/distance_urban_2000.tif"),
    # distance_mine_2000         = paste0(fineprint_grid_30sec_path, "/distance_mine_2000.tif"),
    accessibility_cities_2015 = paste0(fineprint_grid_30sec_path, "/accessibility_to_cities_2015.tif"), 
    countries = paste0(fineprint_grid_30sec_path, "/countries.tif")
  )

# --------------------------------------------------------------------------------------
# get path dir data sets 
pixel_area_vrt <- path.expand(paste0(data_path, "/hansen/pixel_area/area.vrt"))
forest_loss_vrt <- path.expand(paste0(data_path, "/hansen/v1.7/lossyear/lossyear.vrt"))
treecover2000_vrt <- path.expand(paste0(data_path, "/hansen/v1.7/treecover2000/treecover2000.vrt"))

# --------------------------------------------------------------------------------------
# 2. Get processing tiles 
processing_tiles <- dir(pixel_area_dir, pattern = ".tif$", full.name = TRUE) %>% 
  tibble::tibble(tile = .) %>% 
  dplyr::transmute(job_id = dplyr::row_number(), id_hansen = stringr::str_match(tile, "/Hansen_GFC-2017-v1.5_lossyear_(.*?).tif")[,2], tile = tile) %>% 
  dplyr::mutate(id_hansen = stringr::str_remove_all(id_hansen, "_")) %>%
  dplyr::filter(job_id %in% unlist(ifelse(exists("cluster_job_id"), cluster_job_id, list(job_id)))) %>%
  dplyr::mutate(extent = list(raster::extent(raster::raster(tile)))) %>% 
  dplyr::mutate(pixel_area_vrt, forest_loss_vrt, treecover2000_vrt) %>% 
  dplyr::mutate(grid_30sec = list(raster::stack(grid_file_paths))) %>% 
  dplyr::mutate(out_file = purrr::pmap_chr(.l = list(job_id, id_hansen, extent, 
                                                     pixel_area_vrt, forest_loss_vrt, treecover2000_vrt, grid_30sec), 
                                           .f = build_forest_30sec_grid, 
                                           mine_polygons = sf::st_read(mine_polygons, quiet = TRUE), 
                                           country_codes = readr::read_csv(country_codes),
                                           grid_path = fineprint_grid_30sec_path, 
                                           output_path = output_path, 
                                           forest_cover_threshold = forest_cover_threshold,
                                           ncores = 1)) 


