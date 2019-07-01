build_forest_30sec_grid <- function(job_id, id_hansen, area, year, grid_30sec, sf_list, output_path, mine_polygons, log_file = NULL, ncores = 1){
  
  # job_id <- processing_tiles$job_id[[1]]
  # id_hansen <- processing_tiles$id_hansen[[1]]
  # area <- processing_tiles$area[[1]]
  # year <- processing_tiles$year[[1]]
  # treecover2000 <- processing_tiles$treecover2000[[1]]
  # grid_30sec <- processing_tiles$grid_30sec[[1]]
  # output_path <- paste0(fineprint_grid_30sec_path, "/test_processing")
  # mine_polygons <- sf::st_read(mine_polygons)
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
  # start files to write raster grid output 
  tile_dir <- paste0(output_path, "/", id_hansen)
  dir.create(tile_dir, showWarnings = FALSE, recursive = TRUE)
  r_forest_area <- raster::writeStart(raster::raster(grid_30sec), filename = paste0(tile_dir, "/forest_area_2000.tif"), overwrite = TRUE)
  r_forest_area_loss <- raster::writeStart(raster::raster(grid_30sec), filename = paste0(tile_dir, "/forest_area_loss_2017.tif"), overwrite = TRUE)
  r_mine_lease_area <- raster::writeStart(raster::raster(grid_30sec), filename = paste0(tile_dir, "/mine_lease_area.tif"), overwrite = TRUE)
  r_mine_lease_forest_2000 <- raster::writeStart(raster::raster(grid_30sec), filename = paste0(tile_dir, "/mine_lease_forest_2000.tif"), overwrite = TRUE)
  r_mine_lease_forest_loss <- raster::writeStart(raster::raster(grid_30sec), filename = paste0(tile_dir, "/mine_lease_forest_loss.tif"), overwrite = TRUE)
  
  # --------------------------------------------------------------------------------------
  #  block_values <- parallel::mclapply(1:r_blocks$n, mc.cores = ncores, function(b){
  block_values <- lapply(1:r_blocks$n, function(b){
    
    # --------------------------------------------------------------------------------------
    # files to write grid data chunks output 
    fname_grid_sf <- paste0(tile_dir, "/", stringr::str_pad(string = b, width = 4, pad = "0"), "_grid.geojson")
    fname_forest_ts <- paste0(tile_dir, "/", stringr::str_pad(string = b, width = 4, pad = "0"), "_timeseries.csv")
    
    if(file.exists(fname_grid_sf) && file.exists(fname_forest_ts)){
      return(TRUE)
    }
    
    # cat(paste0("\nProcessing block ", b, "/", r_blocks$n,""), file = log_file, append = TRUE)
    cat(paste0("\nProcessing block ", b, "/", r_blocks$n,""))
    
    # --------------------------------------------------------------------------------------
    # Get sub tile extent 
    sub_tile_extent <- raster::extent(tile, r_blocks$row[b], r_blocks$row[b] + r_blocks$nrows[b] - 1, 1, ncol(tile)) 
    # test_set <- mine_polygons %>% dplyr::filter(ECO_NAME == "Xingu-Tocantins-Araguaia moist forests") 
    # sub_tile_extent <- st_bbox(test_set) %>% as.vector() %>% raster::extent()
    
    # --------------------------------------------------------------------------------------
    # project raster to equal-area 
    sub_tile_grid <- raster::crop(grid_30sec, y = sub_tile_extent) 
    layer_names <- names(sub_tile_grid)
    
    # --------------------------------------------------------------------------------------
    # get 30 sec grid for sub tile 
    sub_tile_tbl <- sub_tile_grid %>% 
      stars::st_as_stars() %>% 
      sf::st_as_sf(x, merge = FALSE, as_points = FALSE)
    
    names(sub_tile_tbl) <- c(layer_names, "geometry")
    
    sub_tile_tbl <- sub_tile_tbl %>% 
      tibble::rowid_to_column(var = "id") %>% 
      dplyr::mutate(id_grid = stringr::str_pad(string = id, width = 5, pad = "0")) %>% 
      dplyr::mutate(id_grid = paste0(id_hansen, stringr::str_pad(string = b, width = 4, pad = "0"), "B", id_grid)) 
    
    if(nrow(sub_tile_tbl) < 1){
      return(NULL)
    }
    
    # --------------------------------------------------------------------------------------
    # build velox object 
    forest_2000_velox <- raster::subset(tile, c("area", "treecover2000")) %>% 
      raster::crop(sub_tile_extent) %>% 
      velox::velox()
    
    velox_tile <- raster::crop(tile, sub_tile_extent) %>% 
      velox::velox() 
    
    # --------------------------------------------------------------------------------------
    # aggregate forest area in 2000 to 30sec grid 
    forest_2000 <- forest_2000_velox$extract(sp = sub_tile_tbl, df = TRUE) %>% 
      tibble::as_tibble() %>% 
      dplyr::rename_all(list(~make.names(c("id", "area", "treecover2000")))) %>% 
      dplyr::group_by(id) %>% 
      dplyr::summarise(area_2000 = sum(as.numeric(area) * as.numeric(treecover2000) / 100, na.rm = TRUE)) %>% 
      dplyr::mutate(area_2000 = units::set_units(area_2000, "km^2")) %>% 
      dplyr::mutate(area_2000 = units::set_units(area_2000, "m^2"))
    
    # --------------------------------------------------------------------------------------
    # calculate forest loss area time series for 30sec grid 
    forest_timeseries <- velox_tile$extract(sp = sub_tile_tbl, df = TRUE) %>% 
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
      dplyr::mutate(area_loss = units::set_units(area_loss, "km^2")) %>% 
      dplyr::mutate(area_loss = units::set_units(area_loss, "m^2")) %>% 
      dplyr::mutate(accumulated_loss = units::set_units(accumulated_loss, "km^2")) %>% 
      dplyr::mutate(accumulated_loss = units::set_units(accumulated_loss, "m^2")) %>% 
      dplyr::left_join(forest_2000, by = c("id" = "id")) %>% 
      dplyr::mutate(area_forest = ifelse(year == 0, area_2000, area_2000 - accumulated_loss), year = year + 2000) %>% 
      dplyr::select(-area_2000) 
    
    # --------------------------------------------------------------------------------------
    # get mine polygons intersecting grid 
    mine_intersecting_grid <- mine_polygons %>% 
      sf::st_transform(crs = "+proj=moll") %>% 
      dplyr::filter(lengths(sf::st_intersects(., sf::st_transform(sub_tile_tbl, crs = "+proj=moll"), sparse = TRUE)) > 0)
    
    # --------------------------------------------------------------------------------------
    # calculate forest loss within mine 
    mine_forest_loss_time_series <- 
      tibble::tibble(id_grid = character(), year = double(), accumulated_loss_mine_lease = double(), area_forest_mine_lease = double()) 
    forest_mine <- 
      tibble::tibble(area_forest_2000_mine_lease = double(), area_accumulated_loss_mine_lease = double(), id_grid = character(), area_mine = double())
    
    if(nrow(mine_intersecting_grid) > 0){
      
      # get intersecting grid cells 
      grid_intersecting <- sub_tile_tbl %>% 
        dplyr::select(id_grid) %>% 
        sf::st_transform(crs = "+proj=moll") %>% 
        dplyr::filter(lengths(sf::st_intersects(., mine_intersecting_grid, sparse = TRUE)) > 0)
      
      # calculate mining area for mine intersection with grid cells 
      mine_grid_intersection <- sub_tile_tbl %>% 
        dplyr::select(id_grid) %>% 
        sf::st_transform(crs = "+proj=moll") %>% 
        dplyr::filter(lengths(sf::st_intersects(., mine_intersecting_grid, sparse = TRUE)) > 0) %>% 
        sf::st_intersection(sf::st_geometry(mine_intersecting_grid)) %>% 
        dplyr::group_by(id_grid) %>% 
        dplyr::summarise() %>% 
        dplyr::mutate(area_mine = sf::st_area(geometry), id = dplyr::row_number()) %>% 
        sf::st_cast("MULTIPOLYGON")

      # calculate forest loss time series from direct mining within grid cells 
      mine_forest_2000 <- forest_2000_velox$extract(sp = sf::st_transform(mine_grid_intersection, crs = "+proj=longlat"), df = TRUE) %>% 
        tibble::as_tibble() %>% 
        dplyr::rename_all(list(~make.names(c("id", "area", "treecover2000")))) %>% 
        dplyr::group_by(id) %>% 
        dplyr::summarise(area_2000 = sum(as.numeric(area) * as.numeric(treecover2000) / 100, na.rm = TRUE)) %>% 
        dplyr::mutate(area_2000 = units::set_units(area_2000, "km^2")) %>% 
        dplyr::mutate(area_2000 = units::set_units(area_2000, "m^2"))
      
      mine_forest_loss_time_series <- velox_tile$extract(sp = sf::st_transform(mine_grid_intersection, crs = "+proj=longlat"), df = TRUE) %>%
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
        dplyr::mutate(area_loss = units::set_units(area_loss, "km^2")) %>% 
        dplyr::mutate(area_loss = units::set_units(area_loss, "m^2")) %>% 
        dplyr::mutate(accumulated_loss = units::set_units(accumulated_loss, "km^2")) %>% 
        dplyr::mutate(accumulated_loss = units::set_units(accumulated_loss, "m^2")) %>% 
        dplyr::left_join(mine_forest_2000, by = c("id" = "id")) %>% 
        dplyr::mutate(area_forest = ifelse(year == 0, area_2000, area_2000 - accumulated_loss), year = year + 2000) %>% 
        dplyr::select(-area_2000) 
      
      # calculate total mine forest 
      forest_mine <- mine_forest_loss_time_series %>% 
        dplyr::select(-area_loss) %>% 
        dplyr::filter(year %in% c(2000, 2017)) %>% 
        dplyr::group_by(id) %>% 
        dplyr::summarise(area_forest_2000_mine_lease = area_forest[year == 2000], 
                         area_accumulated_loss_mine_lease = accumulated_loss[year == 2017]) %>% 
        dplyr::left_join(sf::st_drop_geometry(mine_grid_intersection), by = c("id" = "id")) %>% 
        dplyr::select(-id)
      
      mine_forest_loss_time_series <- mine_forest_loss_time_series %>% 
        dplyr::left_join(sf::st_drop_geometry(mine_grid_intersection), by = c("id" = "id")) %>% 
        dplyr::select(id_grid, year, accumulated_loss_mine_lease = area_loss, 
                      area_forest_mine_lease = accumulated_loss)
      
    }
    
    # --------------------------------------------------------------------------------------
    # calculate total forest area 
    forest <- forest_timeseries %>% 
      dplyr::filter(year %in% c(2000, 2017)) %>% 
      dplyr::group_by(id) %>% 
      dplyr::summarise(area_forest_2000 = area_forest[year == 2000], 
                       area_accumulated_forest_loss = accumulated_loss[year == 2017])
    
    # --------------------------------------------------------------------------------------
    # replace local id with global id_grid
    out_forest_timeseries <- sub_tile_tbl %>% 
      tibble::as_tibble() %>% 
      dplyr::select(id, id_grid) %>% 
      dplyr::right_join(forest_timeseries, by = c("id" = "id")) %>% 
      dplyr::select(-id, area_loss) %>% 
      dplyr::left_join(mine_forest_loss_time_series, by = c("id_grid" = "id_grid", "year" = "year")) %>% 
      tidyr::replace_na(list(area_forest_mine_lease = 0, accumulated_loss_mine_lease = 0)) 
      
    out_sub_tile_tbl <- sub_tile_tbl %>% 
      dplyr::left_join(forest, by = c("id", "id")) %>% 
      dplyr::left_join(forest_mine, by = c("id_grid", "id_grid")) %>% 
      tidyr::replace_na(list(area_forest_2000_mine_lease = 0, area_accumulated_loss_mine_lease = 0, area_mine = 0)) 
    
    # --------------------------------------------------------------------------------------
    # write results to file 
    readr::write_csv(out_forest_timeseries, path = fname_forest_ts)
    out_sub_tile_tbl %>% 
      dplyr::select(-id) %>% 
      sf::st_write(dsn = fname_grid_sf, factorsAsCharacter = TRUE, delete_dsn = TRUE)
    
    # fill gaps with zero 
    r_out <- out_sub_tile_tbl %>% 
      dplyr::transmute(id = id,
                       area_forest_2000 = as.numeric(area_forest_2000), 
                       area_accumulated_forest_loss = as.numeric(area_accumulated_forest_loss), 
                       area_forest_2000_mine_lease = as.numeric(area_forest_2000_mine_lease), 
                       area_accumulated_loss_mine_lease = as.numeric(area_accumulated_loss_mine_lease), 
                       area_mine = as.numeric(area_mine)) %>% 
      tidyr::complete(id = full_seq(id, 1), fill = list(area_forest_2000 = 0, 
                                                        area_accumulated_forest_loss = 0, 
                                                        area_forest_2000_mine_lease = 0, 
                                                        area_accumulated_loss_mine_lease = 0, 
                                                        area_mine = 0))
    
    raster::writeValues(r_forest_area, r_out$area_forest_2000, start = r_blocks$row[b])
    raster::writeValues(r_forest_area_loss, r_out$area_accumulated_forest_loss, start = r_blocks$row[b])
    raster::writeValues(r_mine_lease_area, r_out$area_mine, start = r_blocks$row[b])
    raster::writeValues(r_mine_lease_forest_2000, r_out$area_forest_2000_mine_lease, start = r_blocks$row[b])
    raster::writeValues(r_mine_lease_forest_loss, r_out$area_accumulated_loss_mine_lease, start = r_blocks$row[b])
    
    # cat(paste0("\nTile ", id_hansen, " done! ", capture.output(Sys.time() - start_time)), file = log_file, append = TRUE)
    cat(paste0("\nTile ", id_hansen, " subtile ", b, " done! ", capture.output(Sys.time() - start_time)))
    
    return(TRUE)
    
  })
  
  r_forest_area <- raster::writeStop(r_forest_area)
  r_forest_area_loss <- raster::writeStop(r_forest_area_loss)
  r_mine_lease_area <- raster::writeStop(r_mine_lease_area)
  r_mine_lease_forest_2000 <- raster::writeStop(r_mine_lease_forest_2000)
  r_mine_lease_forest_loss <- raster::writeStop(r_mine_lease_forest_loss)
  
  cat(paste0("\nTile ", id_hansen, " done! ", capture.output(Sys.time() - start_time)))
  return(TRUE)
  
}


