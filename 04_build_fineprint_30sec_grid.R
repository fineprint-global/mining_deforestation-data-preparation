library(tidyverse)
library(magrittr)
library(raster)
library(velox)
library(fasterize)
library(sf)
library(parallel)
source("./R/build_forest_30sec_grid.R")
raster::rasterOptions(tmpdir = "./raster_tmp/")

# --------------------------------------------------------------------------------------
# set path to data sets 
if(!exists("data_path"))
  data_path <- "/mnt/nfs_fineprint/data/geoserver"

# --------------------------------------------------------------------------------------
# set output file 
fineprint_grid_30sec_path <- path.expand(paste0(data_path, "/fineprint_grid_30sec"))
dir.create(fineprint_grid_30sec_path, showWarnings = FALSE, recursive = TRUE)

# --------------------------------------------------------------------------------------
# path to 30sec grid files 
pop_2000 <- path.expand(paste0(fineprint_grid_30sec_path, "/population_density_2000.tif"))
elevation <- path.expand(paste0(fineprint_grid_30sec_path, "/elevation.tif"))
slope <- path.expand(paste0(fineprint_grid_30sec_path, "/slope.tif"))
soilgrid <- path.expand(paste0(fineprint_grid_30sec_path, "/soilgrid.tif"))
esa_cci_2000 <- path.expand(paste0(fineprint_grid_30sec_path, "/esa_cci_2000.tif"))
distance_waterway_canal <- path.expand(paste0(fineprint_grid_30sec_path, "/distance_waterway_canal.tif"))
distance_waterway_river <- path.expand(paste0(fineprint_grid_30sec_path, "/distance_waterway_river.tif"))
distance_highway_primary <- path.expand(paste0(fineprint_grid_30sec_path, "/distance_highway_primary.tif"))
distance_highway_motorway <- path.expand(paste0(fineprint_grid_30sec_path, "/distance_highway_motorway.tif"))
distance_highway_secondary <- path.expand(paste0(fineprint_grid_30sec_path, "/distance_highway_secondary.tif"))
distance_highway_trunk <- path.expand(paste0(fineprint_grid_30sec_path, "/distance_highway_trunk.tif"))
distance_mine <- path.expand(paste0(fineprint_grid_30sec_path, "/distance_mine.tif"))
distance_protected_area <- path.expand(paste0(fineprint_grid_30sec_path, "/distance_protected_areas.tif"))
distance_urban_2000 <- paste0(fineprint_grid_30sec_path, "/distance_urban_2000.tif")
distance_mine_2000 <- paste0(fineprint_grid_30sec_path, "/distance_mine_2000.tif")
accessibility_cities_2015 <- paste0(fineprint_grid_30sec_path, "/accessibility_to_cities_2015.tif")
countries <- paste0(fineprint_grid_30sec_path, "/countries.tif")
mine_polygons <- paste0(data_path, "/mine_polygons/mine_polygons_v1r3.geojson")

# --------------------------------------------------------------------------------------
# get path dir data sets 
pixel_area_dir <- path.expand(paste0(data_path, "/hansen/pixel_area"))
forest_loss_dir <- path.expand(paste0(data_path, "/hansen/lossyear"))
treecover2000_dir <- path.expand(paste0(data_path, "/hansen/treecover2000"))

# --------------------------------------------------------------------------------------
# 2. Get processing tiles 
processing_tiles <- dir(pixel_area_dir, pattern = ".tif$", full.name = TRUE) %>% 
  tibble::tibble(area = .) %>% 
  dplyr::transmute(job_id = dplyr::row_number(), id_hansen = stringr::str_match(area, "/Hansen_GFC-2017-v1.5_lossyear_(.*?).tif")[,2], area = area) %>% 
  dplyr::mutate(id_hansen = stringr::str_remove_all(id_hansen, "_")) %>% 
  dplyr::filter(job_id %in% unlist(ifelse(exists("cluster_job_id"), cluster_job_id, list(job_id)))) %>% # Get tile for cluster jobs 
  dplyr::mutate(year = paste0(forest_loss_dir, "/", basename(area))) %>% 
  dplyr::mutate(treecover2000 = paste0(treecover2000_dir, "/", stringr::str_replace(string = basename(area), pattern = "lossyear", replacement = "treecover2000"))) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(area = list(raster::stack(c(area = area))), 
                year = list(raster::stack(c(year = year))),
                treecover2000 = list(raster::stack(c(treecover2000 = treecover2000)))) %>% 
  dplyr::mutate(geometry = sf::st_as_sfc(sf::st_bbox(area), sf::st_crs(area))) %>% 
  sf::st_as_sf() %>% 
  dplyr::ungroup()

# --------------------------------------------------------------------------------------
# 4. Get data sets 
processing_tiles <- processing_tiles %>% 
  dplyr::mutate(grid_30sec = list(raster::stack(c(elevation = elevation,
                                                  slope = slope,
                                                  soilgrid = soilgrid,
                                                  esa_cci_2000 = esa_cci_2000,
                                                  pop_2000 = pop_2000,
                                                  distance_waterway_canal = distance_waterway_canal,
                                                  distance_waterway_river = distance_waterway_river,
                                                  distance_highway_primary = distance_highway_primary,
                                                  distance_highway_motorway = distance_highway_motorway,
                                                  distance_highway_secondary = distance_highway_secondary,
                                                  distance_highway_trunk = distance_highway_trunk, 
                                                  distance_mine = distance_mine,
                                                  distance_urban_2000 = distance_urban_2000,
                                                  distance_mine_2000 = distance_mine_2000,
                                                  accessibility_cities_2015 = accessibility_cities_2015, 
                                                  countries = countries)))) %>% 
  dplyr::mutate(grid_30sec = lapply(seq_along(area), FUN = function(i) raster::crop(grid_30sec[[i]], y = raster::extent(area[[i]]))))

for(i in seq_along(processing_tiles$grid_30sec)){
  names(processing_tiles$grid_30sec[[i]]) <- c("elevation", "slope", "soilgrid", "esa_cci_2000", "pop_2000", "distance_waterway_canal", "distance_waterway_river", 
                                               "distance_highway_primary", "distance_highway_motorway", "distance_highway_secondary", "distance_highway_trunk", 
                                               "distance_mine", "distance_urban_2000", "distance_mine_2000", "accessibility_cities_2015", "countries")
}

# --------------------------------------------------------------------------------------
# 4. Check tiles already processed 
# tiles_done <- dir(fineprint_grid_30sec_path, pattern = ".csv") %>% 
#   stringr::str_replace_all(".csv", "") %>% 
#   stringr::str_split("_") %>% 
#   sapply(tail, n = 1)

# 5. Aggregate Hansens forest loss to 30 arc-sec grid 
tiles_aggregated_forest_loss <- processing_tiles %>% 
  dplyr::mutate(out_file = purrr::pmap_chr(.l = list(job_id, id_hansen, area, year, 
                                                     treecover2000, grid_30sec), 
                                           .f = build_30sec_grid, 
                                           mine_polygons = sf::st_read(mine_polygons), 
                                           output_path = fineprint_grid_30sec_path, 
                                           ncores = 1))


