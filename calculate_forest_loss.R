library(tidyverse)
library(raster)
library(sf)
library(fasterize)
library(velox)
library(fineprintutils)
source("./R/process_hansen_parallel.R")

# 0. Read/Load forest loss tiles 
tiles_forest_loss <- dir("/mnt/nfs_fineprint/tmp/hansen_pixel_area", pattern = ".tif$", full.name = TRUE) %>% 
  tibble::tibble(area = .) %>% 
  dplyr::mutate(year = paste0("/mnt/nfs_fineprint/tmp/hansen/lossyear/", basename(area))) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(r = list(raster::stack(c(year = year, area = area)))) %>% 
  dplyr::mutate(id_tile = stringr::str_match(year, "/Hansen_GFC-2017-v1.5_lossyear_(.*?).tif")[,2]) %>% 
  dplyr::mutate(id_tile = stringr::str_remove_all(id_tile, "_")) %>% 
  dplyr::mutate(geometry = sf::st_as_sfc(sf::st_bbox(r), sf::st_crs(r))) %>% 
  sf::st_as_sf() %>% 
  dplyr::ungroup()

# 1. Read/Load GTOPO30
tiles_gtopo <- dir("/mnt/nfs_fineprint/tmp/GTOPO30", pattern = ".tif$", full.name = TRUE) %>% 
  tibble::tibble(dem = .) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(id_tile = stringr::str_match(dem, "/gt30(.*?).tif")[,2]) %>% 
  dplyr::mutate(id_tile = stringr::str_to_upper(id_tile)) %>% 
  dplyr::ungroup()

# 3. Read/Load ecoregions and protected areas
sf_list = list(ecoregion = sf::read_sf("/mnt/nfs_fineprint/tmp/ecoregions/Ecoregions2017.shp") %>% 
                 dplyr::select(attr = ECO_ID), 
               protected = sf::read_sf("/mnt/nfs_fineprint/tmp/protected_areas/WDPA_Feb2019-shapefile-polygons.shp") %>% 
                 dplyr::select(attr = STATUS_YR))

# For each GTOPO30 tile calculate deforestation 
processing_log <- tiles_gtopo %>% 
  dplyr::mutate(log = purrr::map2(.x = dem,                 
                                 .y = id_tile,
                                 .f = process_hansen_parallel, 
                                 sf_list = sf_list,
                                 r_tbl = tiles_forest_loss,
                                 output_path = "/mnt/nfs_fineprint/tmp/deforestation",
                                 ncores = 7))

