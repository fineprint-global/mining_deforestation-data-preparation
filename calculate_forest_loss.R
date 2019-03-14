library(tidyverse)
library(raster)
library(sf)
library(fasterize)
library(velox)
library(fineprintutils)

# 0. Read/Load forest loss tiles 
tiles_forest_loss <- dir("/mnt/nfs_fineprint/tmp/hansen_pixel_area", pattern = ".tif$", full.name = TRUE) %>% 
  tibble::tibble(area = .) %>% 
  dplyr::mutate(year = paste0("/mnt/nfs_fineprint/tmp/hansen/lossyear/", basename(area))) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(r = list(raster::stack(c(year = year, area = area)))) %>% 
  dplyr::mutate(id_tile = stringr::str_match(year, "/Hansen_GFC-2017-v1.5_lossyear_(.*?).tif")[,2]) %>% 
  dplyr::mutate(id_tile = stringr::str_remove_all(id_tile, "_")) %>% 
  dplyr::mutate(geometry = sf::st_as_sfc(sf::st_bbox(r), sf::st_crs(r))) %>% 
  sf::st_as_sf()

# 1. Read/Load GTOPO30
tiles_gtopo <- dir("/mnt/nfs_fineprint/tmp/GTOPO30", pattern = ".tif$", full.name = TRUE) %>% 
  tibble::tibble(dem = .) %>% 
  dplyr::rowwise() %>% 
  # dplyr::mutate(r = list(raster::raster(dem))) %>% 
  dplyr::mutate(id_tile = stringr::str_match(dem, "/gt30(.*?).tif")[,2]) %>% 
  dplyr::mutate(id_tile = stringr::str_to_upper(id_tile)) 

# 3. Read/Load ecoregions and protected areas
sf_list = list(ecoregion = sf::read_sf("/mnt/nfs_fineprint/tmp/ecoregions/Ecoregions2017.shp") %>% 
                 dplyr::select(attr = ECO_ID), 
               protected = sf::read_sf("/mnt/nfs_fineprint/tmp/protected_areas/WDPA_Feb2019-shapefile-polygons.shp") %>% 
                 dplyr::select(attr = STATUS_YR))

# For each GTOPO30 tile calculate deforestation 
processing_log <- tiles_gtopo %>% 
  dplyr::filter(id_tile == "W060N40") %>%
  dplyr::mutate(log = purrr::map2(.x = dem, 
                                  .y = id_tile,
                                  .f = fun, 
                                  sf_list = sf_list,
                                  r_tbl = tiles_forest_loss)) 

fun <- function(x, y, sf_list, r_tbl){
  
  # 0. Make 30 arc-sec grid from GTOPO30
  # x <- tiles_gtopo$dem[16]
  fname <- basename(x)
  tile_grid <- stars::read_stars(x) %>% 
    sf::st_as_sf(x, merge = FALSE, as_points = FALSE) %>% 
    dplyr::rename(elevation:=!!fname)

  # 1. Save grid to database 
  # REPLACE WITH POSTGIS CONNECTION 
  sf_fpath <- fname %>% 
    stringr::str_replace(pattern = ".tif", replacement = ".gpkg") %>% 
    stringr::str_glue("/mnt/nfs_fineprint/tmp/GTOPO30/", .) 
  
  sf::st_write(obj = tile_grid, dsn = sf_fpath)
  
  # 2. Filter forest tiles overlapping GTOPO30 
  tiles_subset <- r_tbl %>% 
    dplyr::slice(which(sapply(sf::st_intersects(r_tbl, tile_grid), function(i) length(i)>0))) %>% 
    dplyr::filter()
  
  # 3. For each forest tile extract attrubutes and inset to database 
  XXXXXXXXX <- lapply(tiles_subset$r, function(tile){
    # tile <- tiles_subset$r[[7]]
    r_blocks <- raster::blockSize(tile)
    
    # MISSING PARALLEL PROCESSING
    # raster::beginCluster(n = 4)
    t_per_tile <- system.time(
      out_tile <- lapply(1:r_blocks$n, function(b){
        # tile <- tiles_subset$r[[7]]
        # b <- 1
        print(cat(b, "/", r_blocks$n))
        
        print("Check for overlapping grid cells")
        sub_tile_extent <- raster::extent(tile, r_blocks$row[b], r_blocks$row[b] + r_blocks$nrows[b] - 1, 1, ncol(tile))
        sub_tile_bbox <- sf::st_bbox(sub_tile_extent) %>% 
          sf::st_as_sfc() %>% 
          sf::st_sfc(crs = sf::st_crs(tile))
        idx <- sf::st_intersects(sub_tile_bbox, tile_grid)
        if(length(idx[[1]]) < 1){
          return(NULL)
        }
        
        print("Filter intersecting cells")
        subset_tile_grid <- tile_grid %>% 
          dplyr::slice(idx[[1]]) %>% 
          dplyr::filter() %>% 
          dplyr::mutate(id = 1:n(), id_grid = paste0(y, "G", idx[[1]]))
        
        print("Crop raster")
        sub_tile <- raster::crop(tile, y = subset_tile_grid)
        
        print("Rasterize and create velox")
        velox_tile <- raster::stack(c(sub_tile, lapply(sf_list, fasterize::fasterize, raster = raster::raster(sub_tile), field = "attr", fun = "last"))) %>% 
          velox::velox()  
        
        print("Extract from raster")
        r_values <- velox_tile$extract(sp = subset_tile_grid, df = TRUE) %>% 
          tibble::as_tibble() %>% 
          dplyr::rename_all(dplyr::funs(make.names(c("id", names(tile), names(sf_list))))) %>% 
          dplyr::filter(!is.na(ecoregion), !is.na(protected)) %>% 
          dplyr::left_join(subset_tile_grid, by = c("id" = "id")) %>% 
          dplyr::mutate(id_grid = as.character(id_grid), 
                        year = as.integer(year), 
                        ecoregion = as.integer(ecoregion), 
                        protected = as.integer(protected),
                        elevation = as.numeric(elevation),
                        area = as.numeric(area)) %>% 
          dplyr::group_by(id_grid, year, ecoregion, protected) %>% 
          dplyr::summarise(elevation = mean(elevation, na.rm = TRUE), area = sum(area, na.rm = T)) %>% 
          dplyr::ungroup()
        
        return(r_values)
        
      })
    )
    # raster::endCluster()
    
  })
  
  # 2. Rasterize ecoregions
  # t_rasterize_eco <- system.time(
  #   r_eco <- fasterize::fasterize(sf = ecoregions_sf, raster = raster::raster(r), background = -1, field = "ECO_ID", fun = "last")
  # )
  # t_rasterize_pro <- system.time(
  #   r_pro <- fasterize::fasterize(sf = protected_areas_sf, raster = raster::raster(r), background = -1, field = "STATUS_YR", fun = "last")
  # )
  
  # result = tryCatch({
  #   expr
  # }, warning = function(w) {
  #   warning-handler-code
  # }, error = function(e) {
  #   error-handler-code
  # }, finally = {
  #   cleanup-code
  # }
  
  return(x)
         
}




