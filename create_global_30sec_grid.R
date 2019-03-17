library(tidyverse)
library(magrittr)
library(furrr)
library(raster)
library(sf)
source("./R/create_30sec_grid.R")
source("./R/aggregate_forest_loss_to_30sec_grid.R")
gtopo_dir <- "/mnt/nfs_fineprint/tmp/GTOPO30"
pixel_area_dir <- "/mnt/nfs_fineprint/tmp/hansen_pixel_area"
forest_loss_dir <- "/mnt/nfs_fineprint/tmp/hansen/lossyear/"
grid_output_path <- "/mnt/nfs_fineprint/tmp/grid_30sec"
forest_loss_output_path <- "/mnt/nfs_fineprint/tmp/deforestation/"

log_file <- date() %>% 
  stringr::str_replace_all(" ", "_") %>% 
  str_replace_all(":", "") %>% 
  stringr::str_glue(".log") 

# Read/Load ecoregions and protected areas
sf_list = list(ecoregion = sf::read_sf("/mnt/nfs_fineprint/tmp/ecoregions/Ecoregions2017.shp") %>% 
                 dplyr::select(attr = ECO_ID), 
               protected = sf::read_sf("/mnt/nfs_fineprint/tmp/protected_areas/WDPA_Feb2019-shapefile-polygons.shp") %>% 
                 dplyr::select(attr = STATUS_YR))

# Plan cores 
future::plan(future:::multiprocess(workers = 3))

# 1. Create global grid from GTOPO30 tiles 
tiles_gtopo <- dir(gtopo_dir, pattern = ".tif$", full.name = TRUE) %>% 
  tibble::tibble(dem = .) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(id_gtopo = stringr::str_match(dem, "/gt30(.*?).tif")[,2]) %>% 
  dplyr::mutate(id_gtopo = stringr::str_to_upper(id_gtopo)) %>% 
  dplyr::mutate(geometry = list(raster::stack(dem))) %>% 
  dplyr::mutate(geometry = sf::st_as_sfc(sf::st_bbox(geometry), sf::st_crs(geometry))) %>% 
  sf::st_as_sf() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(grid_file = future_map2_chr(.x = dem,
                                            .y = id_gtopo,
                                            .f = create_30sec_grid, 
                                            output_path = grid_output_path,
                                            .progress = TRUE,
                                            log_file = log_file))

# 2. Read deforestation data 
tiles_forest_loss <- dir(pixel_area_dir, pattern = ".tif$", full.name = TRUE) %>% 
  tibble::tibble(area = .) %>% 
  dplyr::mutate(year = paste0(forest_loss_dir, "/", basename(area))) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(geometry = list(raster::stack(c(area = area)))) %>% 
  dplyr::mutate(id_hansen = stringr::str_match(year, "/Hansen_GFC-2017-v1.5_lossyear_(.*?).tif")[,2]) %>% 
  dplyr::mutate(id_hansen = stringr::str_remove_all(id_hansen, "_")) %>% 
  dplyr::mutate(geometry = sf::st_as_sfc(sf::st_bbox(geometry), sf::st_crs(geometry))) %>% 
  sf::st_as_sf() %>% 
  dplyr::ungroup()  
  
# 3. Aggregate Hansens forest loss to 30 arc-sec grid 
tiles_aggregated_forest_loss <- tiles_forest_loss %>% 
  # dplyr::fileter(id_gtopo == job_id) %>% # Used split processing in the WU cluster 
  sf::st_join(y = tiles_gtopo, join = sf::st_covered_by, left = TRUE) %>% 
  dplyr::mutate(out_file = purrr::pmap_chr(.l = list(area, year, grid_file, id_hansen, id_gtopo), 
                                           .f = aggregate_forest_loss_to_30sec_grid,
                                           sf_list = sf_list, 
                                           output_path = forest_loss_output_path, 
                                           log_file = log_file, 
                                           ncores = 12))




