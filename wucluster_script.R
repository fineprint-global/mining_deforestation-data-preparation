library(tidyverse)
library(raster)
library(velox)
library(fasterize)
library(RPostgreSQL)
library(sf)

source("./R/aggregate_forest_loss_to_30sec_grid.R")
gtopo_dir <- path.expand("~/data/gtopo30")
pixel_area_dir <- path.expand("~/data/hansen_pixel_area")
forest_loss_dir <- path.expand("~/data/hansen/lossyear")
treecover2000_dir <- path.expand("~/data/hansen/treecover2000")
ecoregions_path <- path.expand("~/data/ecoregions/Ecoregions2017.shp")
protected_area_path <- path.expand("~/data/protected_areas/WDPA_Apr2019-shapefile-polygons.shp")
forest_loss_output_path <- path.expand("~/data/forest_loss_30sec")

# 1. Read/Load ecoregions and protected areas
sf_list = list(ecoregion = sf::read_sf(ecoregions_path) %>% 
                 dplyr::select(attr = ECO_ID), 
               protected = sf::read_sf(protected_area_path) %>% 
                 dplyr::select(attr = STATUS_YR))

# 2. Read/load GTOPO30 tiles 
tiles_gtopo <- dir(gtopo_dir, pattern = ".tif$", full.name = TRUE) %>% 
  tibble::tibble(dem = .) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(id_gtopo = stringr::str_match(dem, "/gt30(.*?).tif")[,2]) %>% 
  dplyr::mutate(id_gtopo = stringr::str_to_upper(id_gtopo)) %>% 
  dplyr::mutate(geometry = list(raster::stack(dem))) %>% 
  dplyr::mutate(geometry = sf::st_as_sfc(sf::st_bbox(geometry), sf::st_crs(geometry))) %>% 
  sf::st_as_sf() %>% 
  dplyr::ungroup() 

# 3. Read forest 2000 and forest loss 
tiles_forest_loss <- dir(pixel_area_dir, pattern = ".tif$", full.name = TRUE) %>% 
  tibble::tibble(area = .) %>% 
  dplyr::mutate(year = paste0(forest_loss_dir, "/", basename(area))) %>% 
  dplyr::mutate(treecover2000 = paste0(treecover2000_dir, "/", stringr::str_replace(string = basename(area), pattern = "lossyear", replacement = "treecover2000"))) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(geometry = list(raster::stack(c(area = area)))) %>% 
  dplyr::mutate(id_hansen = stringr::str_match(year, "/Hansen_GFC-2017-v1.5_lossyear_(.*?).tif")[,2]) %>% 
  dplyr::mutate(id_hansen = stringr::str_remove_all(id_hansen, "_")) %>% 
  dplyr::mutate(geometry = sf::st_as_sfc(sf::st_bbox(geometry), sf::st_crs(geometry))) %>% 
  sf::st_as_sf() %>% 
  dplyr::ungroup()  

# 4. Check tiles already processed 
# tiles_done <- dir(forest_loss_output_path, pattern = ".csv") %>% 
#   stringr::str_replace_all(".csv", "") %>% 
#   stringr::str_split("_") %>% 
#   sapply(tail, n = 1)

# Get job id 
if(!exists("job_id"))
  job_id <- seq_along(tiles_forest_loss$id_hansen)

log_file <- date() %>% 
  stringr::str_replace_all("  ", "_") %>% 
  stringr::str_replace_all(" ", "_") %>% 
  str_replace_all(":", "") %>% 
  stringr::str_glue("job_id_", job_id, "_" ,.,".log") 

# 5. Aggregate Hansens forest loss to 30 arc-sec grid 
tiles_aggregated_forest_loss <- tiles_forest_loss %>% 
  sf::st_join(y = tiles_gtopo, join = sf::st_covered_by, left = TRUE) %>%
  dplyr::filter(id_hansen == id_hansen[job_id]) %>% # Split jobs among WU cluster nodes 
  dplyr::mutate(out_file = purrr::pmap_chr(.l = list(area, year, id_hansen, id_gtopo, dem), 
                                           .f = aggregate_forest_loss_to_30sec_grid,
                                           sf_list = sf_list, 
                                           output_path = forest_loss_output_path, 
                                           log_file = log_file, 
                                           ncores = 20))

dir.create("./output", showWarnings = FALSE, recursive = TRUE)
sf::write_sf(tiles_aggregated_forest_loss, "./output/tiles_aggregated_forest_loss.gpkg")
