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
  tile_loss <- raster::stack(c(area = area, year = year, treecover2000 = treecover2000))
  tile_2000 <- raster::stack(c(area = area, treecover2000 = treecover2000))
  names(tile_loss) <- c("area", "year", "treecover2000")
  names(tile_2000) <- c("area", "treecover2000")
  
  # --------------------------------------------------------------------------------------
  # Split processing blocks
  r_blocks <- raster::blockSize(tile_loss, minrows = 160)
  grid_fname <- paste0(output_path, "/", id_hansen, ".tif")
  
  # --------------------------------------------------------------------------------------
  # Parallel processing 
  start_time <- Sys.time()
  # cat(paste0("\nProcessing forest loss tile ", id_hansen, " using ", ncores," cores"), file = log_file, append = TRUE)
  cat(paste0("\nProcessing forest loss tile ", id_hansen, " using ", ncores," cores"))
  
  # --------------------------------------------------------------------------------------
  #  block_values <- parallel::mclapply(1:r_blocks$n, mc.cores = ncores, function(b){
  block_values <- lapply(1:r_blocks$n, function(b){
    
    grid_fname <- paste0(output_path, "/", id_hansen, "_", b, ".shp")
    data_fname <- paste0(output_path, "/", id_hansen, "_", b, ".csv")
    
    if(file.exists(grid_fname) && file.exists(data_fname)){
      return(TRUE)
    }
    
    # cat(paste0("\nProcessing block ", b, "/", r_blocks$n,""), file = log_file, append = TRUE)
    cat(paste0("\nProcessing block ", b, "/", r_blocks$n,""))
    
    # --------------------------------------------------------------------------------------
    # Get sub tile extent 
    sub_tile_extent <- raster::extent(tile_loss, r_blocks$row[b], r_blocks$row[b] + r_blocks$nrows[b] - 1, 1, ncol(tile_loss))
    sub_tile_bbox <- sf::st_bbox(sub_tile_extent) %>% 
      sf::st_as_sfc() %>% 
      sf::st_sfc(crs = sf::st_crs(tile_loss)) %>% 
      sf::st_sf()
    
    # --------------------------------------------------------------------------------------
    # Create 30 sec grid for sub tile 
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
        
    # Get sub tile data 
    velox_loss <- raster::crop(tile_loss, sub_tile_extent) %>% 
      velox::velox()
    
    velox_2000 <- raster::crop(tile_2000, sub_tile_extent) %>% 
      velox::velox()
    
    # Aggregate values to 30sec grid 
    r_values_loss <- velox_loss$extract(sp = sub_tile_grid, df = TRUE) %>% 
      tibble::as_tibble() %>% 
      dplyr::rename_all(list(~make.names(c("id", names(tile_loss))))) 
    
    r_values_2000 <- velox_loss$extract(sp = sub_tile_grid, df = TRUE) %>% 
      tibble::as_tibble() %>% 
      dplyr::rename_all(list(~make.names(c("id", names(tile_2000))))) 
    
    forest_2000 <- r_values_2000 %>% 
      dplyr::filter(id == 1429) %>% 
      dplyr::group_by(id) %>% 
      dplyr::summarise(area_2000 = sum(as.numeric(area) * as.numeric(treecover2000) / 100, na.rm = TRUE)) 
  
    forest_loss <- r_values_loss %>%  
      dplyr::filter(id == 1429) %>% 
      dplyr::filter(year != 0) %>% 
      dplyr::group_by(id, year) %>% 
      dplyr::summarise(area_loss = sum(as.numeric(area) * as.numeric(treecover2000) / 100, na.rm = TRUE)) %>% 
      dplyr::arrange(id, year) %>% 
      dplyr::mutate(accumulated_loss = cumsum(area_loss))

    forest_loss
    
      # dplyr::rename_all(list(~make.names(c("id", names(tile))))) %>%
      # dplyr::filter(!near(treecover2000, 0)) %>% 
      # dplyr::left_join(sub_tile_grid, by = c("id" = "id")) %>%
      # dplyr::transmute(id_grid = as.character(id_grid),
      #                  year = as.integer(year + 2000), 
      #                  ecoregion = as.integer(ecoregion),
      #                  ecoregion = ifelse(is.na(ecoregion), 9999, ecoregion),          # Ecoregions NA to 9999
      #                  protected = ifelse(as.integer(protected) <= year, TRUE, FALSE), # Check if deforestation happens after the status of protected
      #                  protected = !is.na(protected),
      #                  area = as.numeric(area) * (as.numeric(treecover2000) / 100)) %>% 
      # dplyr::group_by(id_grid, year, ecoregion, protected) %>%
      # dplyr::summarise(area = sum(area, na.rm = TRUE)) %>% 
      # dplyr::ungroup()
    
    # Calculate forest cover and forest loss time series 
    forest_loss_area <- r_values %>% 
      dplyr::filter(year == 2000) %>% 
      dplyr::rename(area_2000 = area) %>% 
      dplyr::select(-year) %>% 
      dplyr::right_join(r_values, by = c("id_grid" = "id_grid", "ecoregion" = "ecoregion", "protected" = "protected")) %>%  
      dplyr::mutate(area_loss = ifelse(year == 2000, 0, area)) %>% 
      dplyr::group_by(id_grid, ecoregion, protected) %>% 
      dplyr::arrange(id_grid, year, ecoregion, protected) %>% 
      dplyr::mutate(accumulated_loss = cumsum(area_loss)) %>% 
      dplyr::mutate(area_forest = area_2000 - accumulated_loss) %>% 
      dplyr::select(-area_2000, -area) %>% 
      dplyr::ungroup()
    
    # Replace id with global id_grid
    sub_tile_grid <- sub_tile_grid %>% 
      dplyr::select(id = id_grid, elevation)
    
    ### pfusch - replace with DB connection  
    #grid_fname <- paste0(output_path, "/", id_gtopo, "_", id_hansen, "_", b, ".shp")
    #data_fname <- paste0(output_path, "/", id_gtopo, "_", id_hansen, "_", b, ".csv")
    sf::st_write(sub_tile_grid, dsn = grid_fname, factorsAsCharacter = TRUE)
    readr::write_csv(forest_loss_area, path = data_fname)
    
    # cat(paste0("\nTile ", id_hansen, " done! ", capture.output(Sys.time() - start_time)), file = log_file, append = TRUE)
    cat(paste0("\nTile ", id_hansen, " subtile ", b, " done! ", capture.output(Sys.time() - start_time)))
    
    return(TRUE)
    
  })
  
  cat(paste0("\nTile ", id_hansen, " done! ", capture.output(Sys.time() - start_time)))
  return(TRUE)
  
}
