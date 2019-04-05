aggregate_forest_loss_to_30sec_grid <- function(area, year, id_hansen, id_gtopo, dem, sf_list, output_path, log_file, ncores){
  
  # area <- tiles_aggregated_forest_loss$area[10]
  # year <- tiles_aggregated_forest_loss$year[10]
  # treecover2000 <- tiles_aggregated_forest_loss$treecover2000[10]
  # id_hansen <- tiles_aggregated_forest_loss$id_hansen[10]
  # id_gtopo <- tiles_aggregated_forest_loss$id_gtopo[10]
  # dem <- tiles_aggregated_forest_loss$dem[10]
  # output_path <- "/mnt/nfs_fineprint/tmp/grid_30sec/deforestation_TEST"
  
  if(is.null(log_file))
    log_file <- date() %>% 
      stringr::str_replace_all(" ", "_") %>% 
      str_replace_all(":", "") %>% 
      stringr::str_glue(".log") 
  
  dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
  
  # ead tile 
  tile <- raster::stack(list(area = area, year = year, treecover2000 = treecover2000))
  dem_tile <- raster::stack(list(dem = dem))
  
  # Split processing blocks
  r_blocks <- raster::blockSize(tile, minrows = 160)
  
  # Parallel processing 
  start_time <- Sys.time()
  cat(paste0("\nProcessing forest loss tile ", id_hansen, " using ", ncores," cores"), file = log_file, append = TRUE)
  
  block_values <- parallel::mclapply(1:r_blocks$n, mc.cores = ncores, function(b){
    
    cat(paste0("\nProcessing block ", b, "/", r_blocks$n,""), file = log_file, append = TRUE)
    
    # Get sub tile extent 
    sub_tile_extent <- raster::extent(tile, r_blocks$row[b], r_blocks$row[b] + r_blocks$nrows[b] - 1, 1, ncol(tile))
    sub_tile_bbox <- sf::st_bbox(sub_tile_extent) %>% 
      sf::st_as_sfc() %>% 
      sf::st_sfc(crs = sf::st_crs(tile)) %>% 
      sf::st_sf()
    
    # Create 30 sec grid for sub tile 
    sub_tile_grid <- raster::crop(dem_tile, y = sub_tile_bbox) %>% 
      stars::st_as_stars() %>% 
      sf::st_as_sf(x, merge = FALSE, as_points = FALSE) %>% 
      dplyr::rename(elevation = dem) %>% 
      tibble::rowid_to_column(var = "id") %>% 
      dplyr::mutate(id_grid = stringr::str_pad(string = id, width = 4, pad = "0")) %>% 
      dplyr::mutate(id_grid = paste0(id_hansen, stringr::str_pad(string = r_blocks$row[b], width = 5, pad = "0"), "B", id_grid)) 

    if(nrow(sub_tile_grid) < 1){
      return(NULL)
    }
    
    # TODO: Add grid to database 
    
    # Get sub tile data 
    sub_tile <- raster::crop(tile, sub_tile_extent)
    velox_tile <- raster::stack(c(sub_tile, lapply(sf_list, fasterize::fasterize, raster = raster::raster(sub_tile), field = "attr", fun = "last"))) %>% 
      velox::velox()

    # Aggregate values to 30sec grid 
    r_values <- velox_tile$extract(sp = sub_tile_grid, df = TRUE) %>% 
      tibble::as_tibble() %>% 
      dplyr::rename_all(dplyr::funs(make.names(c("id", names(tile), names(sf_list))))) %>%
      dplyr::filter(!near(treecover2000, 0)) %>% 
      dplyr::left_join(sub_tile_grid, by = c("id" = "id")) %>%
      dplyr::mutate(id_grid = as.character(id_grid), 
                    year = as.integer(year), 
                    ecoregion = as.integer(ecoregion),
                    protected = as.integer(protected),
                    elevation = as.numeric(elevation),
                    area = as.numeric(area) * (as.numeric(treecover2000) / 100)) %>% 
      dplyr::group_by(id_grid, year, ecoregion, protected) %>%
      dplyr::summarise(elevation = mean(elevation, na.rm = TRUE), 
                       area = sum(area, na.rm = TRUE)) %>% 
      dplyr::ungroup()
    
    # TODO: Calculate forest area in 2000 and ADD to grid 
    
    # TODO: Remove area in 2000 from tbl 
    
    return(list(geomtry = sub_tile_grid, tbl = r_values))
    
  })
  
  # TODO: Insset to DB 
  gc(verbose = FALSE)
  tile_fname <- paste0(output_path, "/", id_gtopo, "_", id_hansen, ".csv")
  readr::write_csv(dplyr::bind_rows(block_values), path = tile_fname, append = FALSE)
  end_time <- Sys.time()
  cat(paste0("\nTile ", id_hansen, " done! ", capture.output(end_time - start_time)), file = log_file, append = TRUE)

  return(tile_fname)
  
}
