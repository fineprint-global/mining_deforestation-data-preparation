build_30sec_grid <- function(job_id, id_hansen, area, year, grid_30sec, sf_list, output_path, log_file = NULL, ncores = 1){
  
  # job_id <- processing_tiles$job_id[[1]]
  # id_hansen <- processing_tiles$id_hansen[[1]]
  # area <- processing_tiles$area[[1]]
  # year <- processing_tiles$year[[1]]
  # treecover2000 <- processing_tiles$treecover2000[[1]]
  # grid_30sec <- processing_tiles$grid_30sec[[1]]
  # sf_list <- sf_list
  # output_path <- fineprint_grid_30sec_path
  # ncores <- 1
  
  # --------------------------------------------------------------------------------------
  # stack raster files 
  tile <- raster::stack(c(area = area, year = year, treecover2000 = treecover2000))
  names(tile) <- c("area", "year", "treecover2000")
  
  # --------------------------------------------------------------------------------------
  # Split processing blocks
  r_blocks <- raster::blockSize(tile, minrows = 160)
  output_path <- paste0(output_path, "/forest_timeseries")
  dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
  
  # --------------------------------------------------------------------------------------
  # Parallel processing 
  start_time <- Sys.time()
  # cat(paste0("\nProcessing forest loss tile ", id_hansen, " using ", ncores," cores"), file = log_file, append = TRUE)
  cat(paste0("\nProcessing forest loss tile ", id_hansen, " using ", ncores," cores"))
  
  # --------------------------------------------------------------------------------------
  #  block_values <- parallel::mclapply(1:r_blocks$n, mc.cores = ncores, function(b){
  block_values <- lapply(1:r_blocks$n, function(b){
    
    grid_fname <- paste0(output_path, "/", id_hansen, "_", stringr::str_pad(string = b, width = 4, pad = "0"), ".geojson")
    data_fname <- paste0(output_path, "/", id_hansen, "_", stringr::str_pad(string = b, width = 4, pad = "0"), ".csv")
    
    if(file.exists(grid_fname) && file.exists(data_fname)){
      return(TRUE)
    }
    
    # cat(paste0("\nProcessing block ", b, "/", r_blocks$n,""), file = log_file, append = TRUE)
    cat(paste0("\nProcessing block ", b, "/", r_blocks$n,""))
    
    # --------------------------------------------------------------------------------------
    # Get sub tile extent 
    sub_tile_extent <- raster::extent(tile, r_blocks$row[b], r_blocks$row[b] + r_blocks$nrows[b] - 1, 1, ncol(tile))
    sub_tile_bbox <- sf::st_bbox(sub_tile_extent) %>% 
      sf::st_as_sfc() %>% 
      sf::st_sfc(crs = sf::st_crs(tile)) %>% 
      sf::st_sf()
    
    # --------------------------------------------------------------------------------------
    # get 30 sec grid for sub tile 
    sub_tile_grid <- raster::crop(grid_30sec, y = sub_tile_bbox)
                       
    sub_tile_grid <- raster::stack(c(sub_tile_grid,
                                     lapply(sf_list[c("ecoregion")], fasterize::fasterize, raster = raster::raster(sub_tile_grid), field = "attr", fun = "last")))
    
    layer_names <- names(sub_tile_grid)
    
    sub_tile_grid <- sub_tile_grid %>% 
      stars::st_as_stars() %>% 
      sf::st_as_sf(x, merge = FALSE, as_points = FALSE)
    
    names(sub_tile_grid) <- c(layer_names, "geometry")
    
    sub_tile_grid <- sub_tile_grid %>% 
      tibble::rowid_to_column(var = "id") %>% 
      dplyr::mutate(id_grid = stringr::str_pad(string = id, width = 5, pad = "0")) %>% 
      dplyr::mutate(id_grid = paste0(id_hansen, stringr::str_pad(string = b, width = 4, pad = "0"), "B", id_grid)) 
    
    if(nrow(sub_tile_grid) < 1){
      return(NULL)
    }
        
    # Aggregate forest area 2000 to 30sec grid 
    forest_2000 <- raster::subset(tile, c("area", "treecover2000")) %>% 
      raster::crop(sub_tile_extent) %>% 
      velox::velox()
      
    forest_2000 <- forest_2000$extract(sp = sub_tile_grid, df = TRUE) %>% 
      tibble::as_tibble() %>% 
      dplyr::rename_all(list(~make.names(c("id", "area", "treecover2000")))) %>% 
      dplyr::group_by(id) %>% 
      dplyr::summarise(area_2000 = sum(as.numeric(area) * as.numeric(treecover2000) / 100, na.rm = TRUE)) 
    
    velox_tile <- raster::crop(tile, sub_tile_extent) %>% 
      velox::velox() 

    forest_timeseries <- velox_tile$extract(sp = sub_tile_grid, df = TRUE) %>% 
      tibble::as_tibble() %>% 
      dplyr::rename_all(list(~make.names(c("id", names(tile))))) %>% 
      dplyr::mutate(area = as.numeric(area) * as.numeric(treecover2000) / 100) %>% 
      dplyr::group_by(id, year) %>% 
      dplyr::summarise(area_loss = sum(area, na.rm = TRUE)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(area_loss = ifelse(year == 0, 0, area_loss)) %>% 
      tidyr::complete(year = full_seq(year, 1), nesting(id), fill = list(area_loss = 0)) %>% 
      dplyr::group_by(id) %>% 
      dplyr::arrange(year) %>% 
      dplyr::mutate(accumulated_loss = c(0, cumsum(area_loss[year != 0]))) %>% 
      dplyr::ungroup() %>% 
      dplyr::left_join(forest_2000, by = c("id" = "id")) %>% 
      dplyr::mutate(area_forest = ifelse(year == 0, area_2000, area_2000 - accumulated_loss), year = year + 2000) %>% 
      dplyr::select(-area_2000)
      
    # total forest areas 
    forest <- forest_timeseries %>% 
      dplyr::select(-area_loss) %>% 
      dplyr::filter(year %in% c(2000, 2017)) %>% 
      dplyr::group_by(id) %>% 
      dplyr::summarise(area_forest_2000 = area_forest[year == 2000], 
                       area_accumulated_forest_loss = accumulated_loss[year == 2017])
    
    # Replace id with global id_grid
    forest_timeseries <- sub_tile_grid %>% 
      dplyr::select(id, id_grid) %>% 
      dplyr::right_join(forest_timeseries, by = c("id" = "id")) %>% 
      dplyr::select(-id)
    
    sub_tile_grid <- sub_tile_grid %>% 
      dplyr::left_join(forest, by = c("id", "id")) %>% 
      dplyr::select(-id) 
    
    ### pfusch - replace with DB connection  
    sf::st_write(sub_tile_grid, dsn = grid_fname, factorsAsCharacter = TRUE, delete_dsn = TRUE)
    readr::write_csv(forest_timeseries, path = data_fname)
    
    # cat(paste0("\nTile ", id_hansen, " done! ", capture.output(Sys.time() - start_time)), file = log_file, append = TRUE)
    cat(paste0("\nTile ", id_hansen, " subtile ", b, " done! ", capture.output(Sys.time() - start_time)))
    
    return(TRUE)
    
  })
  
  cat(paste0("\nTile ", id_hansen, " done! ", capture.output(Sys.time() - start_time)))
  return(TRUE)
  
}


