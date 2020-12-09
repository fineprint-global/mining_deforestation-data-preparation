build_forest_30sec_grid <- function(job_id, id_hansen, area, year, treecover2000, grid_30sec, mine_polygons, country_codes, log_file = NULL, grid_path, output_path, ncores = 1){

  # job_id <- processing_tiles$job_id[[1]]
  # id_hansen <- processing_tiles$id_hansen[[1]]
  # area <- processing_tiles$area[[1]]
  # year <- processing_tiles$year[[1]]
  # treecover2000 <- processing_tiles$treecover2000[[1]]
  # grid_30sec <- processing_tiles$grid_30sec[[1]]
  # output_path <- paste0(fineprint_grid_30sec_path, "/test_processing")
  # mine_polygons <- sf::st_read(mine_polygons)
  # country_codes <- readr::read_csv(country_codes)
  # ncores <- 1
  
  tile_start_time <- Sys.time()

  # --------------------------------------------------------------------------------------
  # stack raster files 
  tile <- raster::stack(c(area = area, year = year, treecover2000 = treecover2000))
  tile_30sec <- raster::crop(raster::raster(grid_30sec), raster::extent(tile))
  names(tile) <- c("area", "year", "treecover2000")
  
  # --------------------------------------------------------------------------------------
  # Split processing blocks
  # r_blocks <- raster::blockSize(tile, minblocks = 800)
  r_blocks <- raster::blockSize(tile, minrows = 160)
  #output_path <- paste0(output_path, "/timeseries", format(Sys.time(), "_%Y%m%d%H%M%S"))
  #dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
  
  # --------------------------------------------------------------------------------------
  # Parallel processing 
  # cat(paste0("\nProcessing forest loss tile ", id_hansen, " using ", ncores," cores"), file = log_file, append = TRUE)
  cat(paste0("\nProcessing forest loss tile ", id_hansen, " using ", ncores," cores"))
  
  # --------------------------------------------------------------------------------------
  # start files to write raster grid output 
  tile_dir <- paste0(output_path, "/", id_hansen)
  dir.create(tile_dir, showWarnings = FALSE, recursive = TRUE)
  r_forest_area <- raster::writeStart(raster::raster(tile_30sec), filename = paste0(tile_dir, "/forest_area_2000.tif"), overwrite = TRUE)
  r_forest_area_loss_2010 <- raster::writeStart(raster::raster(tile_30sec), filename = paste0(tile_dir, "/forest_area_loss_2010.tif"), overwrite = TRUE)
  r_forest_area_loss_2019 <- raster::writeStart(raster::raster(tile_30sec), filename = paste0(tile_dir, "/forest_area_loss_2019.tif"), overwrite = TRUE)
  r_mine_lease_area <- raster::writeStart(raster::raster(tile_30sec), filename = paste0(tile_dir, "/mine_lease_area.tif"), overwrite = TRUE)
  r_mine_lease_forest_2000 <- raster::writeStart(raster::raster(tile_30sec), filename = paste0(tile_dir, "/mine_lease_forest_2000.tif"), overwrite = TRUE)
  r_mine_lease_forest_loss_2010 <- raster::writeStart(raster::raster(tile_30sec), filename = paste0(tile_dir, "/mine_lease_forest_loss_2010.tif"), overwrite = TRUE)
  r_mine_lease_forest_loss_2019 <- raster::writeStart(raster::raster(tile_30sec), filename = paste0(tile_dir, "/mine_lease_forest_loss_2019.tif"), overwrite = TRUE)
  r_mine_lease_forest_loss_timeseries <- raster::writeStart(raster::brick(tile_30sec, values = FALSE, nl = 20), filename = paste0(tile_dir, "/mine_lease_forest_loss_timeseries.tif"), overwrite = TRUE)
  
  # --------------------------------------------------------------------------------------
  #  block_values <- parallel::mclapply(1:r_blocks$n, mc.cores = ncores, function(b){
  block_values <- lapply(1:r_blocks$n, function(b){
    
    start_time <- Sys.time()

    # cat(paste0("\nProcessing block ", b, "/", r_blocks$n,""), file = log_file, append = TRUE)
    cat(paste0("\n\nProcessing block ", b, "/", r_blocks$n,""))
    
    # --------------------------------------------------------------------------------------
    # files to write grid data chunks output 
    fname_grid_sf <- paste0(tile_dir, "/", stringr::str_pad(string = b, width = 4, pad = "0"), "_grid.geojson")
    fname_forest_ts <- paste0(tile_dir, "/", stringr::str_pad(string = b, width = 4, pad = "0"), "_timeseries.csv")
    
    #if(file.exists(fname_grid_sf) && file.exists(fname_forest_ts)){
    #  return(TRUE)
    #}
    
    # --------------------------------------------------------------------------------------
    # Get sub tile extent 
    sub_tile_extent <- raster::extent(tile, r_blocks$row[b], r_blocks$row[b] + r_blocks$nrows[b] - 1, 1, ncol(tile)) 
    # test_set <- mine_polygons %>% dplyr::filter(ECO_NAME == "Xingu-Tocantins-Araguaia moist forests") 
    # sub_tile_extent <- st_bbox(test_set) %>% as.vector() %>% raster::extent()
    
    # --------------------------------------------------------------------------------------
    # project raster to equal-area 
    row_start <- raster::rowFromCell(tile_30sec, raster::cellsFromExtent(tile_30sec, sub_tile_extent))[1]
    sub_tile_grid <- raster::crop(grid_30sec, y = sub_tile_extent) 
    layer_names <- names(sub_tile_grid)
    
    # --------------------------------------------------------------------------------------
    # get 30 sec grid for sub tile 
    sub_tile_tbl <- sub_tile_grid %>% 
      stars::st_as_stars() %>% 
      sf::st_as_sf(x, merge = FALSE, as_points = FALSE) %>% 
      tibble::as_tibble() %>% 
      sf::st_as_sf()
    
    names(sub_tile_tbl) <- c(layer_names, "geometry")
    
    sub_tile_tbl <- sub_tile_tbl %>% 
      tibble::rowid_to_column(var = "id") %>% 
      dplyr::mutate(id_grid = stringr::str_pad(string = id, width = 5, pad = "0")) %>% 
      dplyr::mutate(id_grid = paste0(id_hansen, stringr::str_pad(string = b, width = 4, pad = "0"), "B", id_grid)) 
    
    if(nrow(sub_tile_tbl) < 1){
      return(NULL)
    }
    
    # --------------------------------------------------------------------------------------
    # redimention stack: year attr to dim 
    # forest_2000_velox <- raster::subset(tile, c("area", "treecover2000")) %>% 
    #   raster::crop(sub_tile_extent) %>% 
    #   stars::st_as_stars()
      # velox::velox()

    fun_forest_area <- function(x){
      # 10% forest cover 2000 threshold - https://developers.google.com/earth-engine/tutorials/community/forest-cover-loss-estimation
      res <- c(ifelse(x[3] >= 10, x[2], 0), rep(NA, 19))
      if( x[1] == 0 ) return(res)
      # get forest loss area
      res[x[1] + 1] <- x[2]
      return(res)
    }
    
    forest_stars <- raster::subset(tile, c("year", "area", "treecover2000")) %>% 
      raster::crop(sub_tile_extent + c(-0.004, 0.004, -0.004, 0.004)) %>% # ~0.5km buffer  
      stars::st_as_stars() %>% 
      stars::st_apply(MARGIN = 1:2, FUN = fun_forest_area)
    
    # a <- forest_stars %>%
    #   slice("fun_forest_area", 1) 
    # sum(a$year, na.rm = T)
    #   plot()
    
    # sub_tile_tbl %>% 
    #   sf::st_drop_geometry() %>% 
    #   tibble::as_tibble() %>% 
    #   dplyr::select(id) %>% 
    #   dplyr::bind_cols(loss_tbl) %>% 
    #   tidyr::pivot_longer(cols = num_range(prefix = "Y", range = 2001:2019), names_to = "year", values_to = "area") %>% 
    #   dplyr::mutate(year = as.numeric(stringr::str_remove(year, "Y")))
    
    # velox_tile <- raster::crop(tile, sub_tile_extent) %>% 
    #   velox::velox()
    
    # --------------------------------------------------------------------------------------
    # aggregate forest area in 2000 to 30sec grid 
    # forest_2000_df <- forest_2000_velox$extract(sp = sub_tile_tbl, df = TRUE)
    
    # brazil <- cbind(brazil, exact_extract(prec, brazil, c('min', 'max')))
      
      
    # forest_stars %>% 
    #   dplyr::slice("fun_forest_area", y) %>% 
    #   as("Raster") %>% 
    #   plot()
        
    loss_tbl <- lapply(1:20, function(y){
      forest_stars %>% 
        dplyr::slice("fun_forest_area", y) %>% 
        as("Raster") %>% 
        exactextractr::exact_extract(y = sub_tile_tbl, fun = 'sum', include_cell = FALSE, progress = FALSE)
    })
    names(loss_tbl) <- c("treecover2000", stringr::str_glue("Y{1:19}"))
    
    forest_2000_df <- sub_tile_tbl %>% 
      sf::st_drop_geometry() %>% 
      tibble::as_tibble() %>% 
      dplyr::select(id) %>% 
      dplyr::bind_cols(loss_tbl)
    
    # sub_tile_tbl %>% 
    #   sf::st_drop_geometry() %>% 
    #   tibble::as_tibble() %>% 
    #   dplyr::select(id) %>% 
    #   dplyr::bind_cols(loss_tbl) %>% 
    #   tidyr::pivot_longer(cols = num_range(prefix = "Y", range = 2001:2019), names_to = "year", values_to = "area") %>% 
    #   dplyr::mutate(year = as.numeric(stringr::str_remove(year, "Y")))
    
    forest_2000 <- 
      tibble::tibble(id = integer(), area_2000 = double()) 
    forest_timeseries <- 
      tibble::tibble(year = integer(), id = integer(), area_loss = double(), accumulated_loss = double(), area_forest = double())
    
    if(tibble::is_tibble(forest_2000_df)){
      if(nrow(forest_2000_df) > 0){
        forest_2000 <- forest_2000_df %>% 
          dplyr::select(id, area_2000 = treecover2000) %>% 
          dplyr::mutate(area_2000 = units::set_units(area_2000, "km^2")) %>% 
          dplyr::mutate(area_2000 = units::set_units(area_2000, "m^2")) 
        
        # --------------------------------------------------------------------------------------
        # calculate forest loss area time series for 30sec grid 
        forest_timeseries <- forest_2000_df %>% 
          tidyr::pivot_longer(cols = num_range(prefix = "Y", range = 1:19), names_to = "year", values_to = "area") %>%
          dplyr::mutate(year = as.numeric(stringr::str_remove(year, "Y"))) %>% 
          dplyr::group_by(id, year) %>% 
          dplyr::summarise(area_loss = sum(area, na.rm = TRUE), .groups = 'drop') %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(area_loss = ifelse(year == 0, 0, area_loss)) %>% 
          tidyr::complete(year = tidyr::full_seq(0:19, 1), nesting(id), fill = list(area_loss = 0)) %>% 
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
      }
    }
    
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
      tibble::tibble(area_forest_2000_mine_lease = double(), area_accumulated_loss_mine_lease_2010 = double(), area_accumulated_loss_mine_lease_2019 = double(), id_grid = character(), area_mine = double())
    out_forest_timeseries <- 
      tibble::tibble(id_grid = character(), year = double(), area_forest_loss_mine_lease = double(), area_forest_mine_lease = double())

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
        dplyr::summarise(.groups = 'drop') %>% 
        dplyr::mutate(area_mine = sf::st_area(geometry), id = dplyr::row_number()) %>% 
        sf::st_cast("MULTIPOLYGON") %>% 
        sf::st_transform(crs = "+proj=longlat")

      # calculate forest loss time series from direct mining within grid cells 
      # mine_forest_2000 <- forest_2000_velox$extract(sp = sf::st_transform(mine_grid_intersection, crs = "+proj=longlat"), df = TRUE) 
      
      loss_tbl <- lapply(1:20, function(y){
        forest_stars %>% 
          dplyr::slice("fun_forest_area", y) %>% 
          as("Raster") %>% 
          exactextractr::exact_extract(y = mine_grid_intersection, fun = 'sum', progress = FALSE)
      })
      names(loss_tbl) <- c("treecover2000", stringr::str_glue("Y{1:19}"))
      
      mine_forest_df <- mine_grid_intersection %>% 
        sf::st_drop_geometry() %>% 
        tibble::as_tibble() %>% 
        dplyr::select(id) %>% 
        dplyr::bind_cols(loss_tbl) 
      
      
      if(tibble::is_tibble(mine_forest_df)){
        if(nrow(mine_forest_df) > 0){
          
          mine_forest_2000 <- mine_forest_df %>% 
            dplyr::select(id, area_2000 = treecover2000) %>% 
            dplyr::mutate(area_2000 = units::set_units(area_2000, "km^2")) %>% 
            dplyr::mutate(area_2000 = units::set_units(area_2000, "m^2")) 
          
          mine_forest_loss_time_series <- mine_forest_df %>% 
            tidyr::pivot_longer(cols = num_range(prefix = "Y", range = 1:19), names_to = "year", values_to = "area") %>%
            dplyr::mutate(year = as.numeric(stringr::str_remove(year, "Y"))) %>% 
            dplyr::group_by(id, year) %>% 
            dplyr::summarise(area_loss = sum(area, na.rm = TRUE), .groups = 'drop') %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(area_loss = ifelse(year == 0, 0, area_loss)) %>% 
            tidyr::complete(year = tidyr::full_seq(0:19, 1), nesting(id), fill = list(area_loss = 0)) %>% 
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
            dplyr::filter(year %in% c(2000, 2010, 2019)) %>% 
            dplyr::group_by(id) %>% 
            dplyr::summarise(area_forest_2000_mine_lease = area_forest[year == 2000],
                             area_accumulated_loss_mine_lease_2010 = accumulated_loss[year == 2010],
                             area_accumulated_loss_mine_lease_2019 = accumulated_loss[year == 2019],
			     .groups = 'drop') %>% 
            dplyr::left_join(sf::st_drop_geometry(mine_grid_intersection), by = c("id" = "id")) %>% 
            dplyr::select(-id)
          
          out_forest_timeseries <- mine_forest_loss_time_series %>% 
            dplyr::left_join(sf::st_drop_geometry(mine_grid_intersection), by = c("id" = "id")) %>% 
            dplyr::select(id_grid, year, area_forest_loss_mine_lease = area_loss, area_forest_mine_lease = area_forest)
	  
	  out_forest_timeseries %>%
            dplyr::left_join(tibble::as_tibble(sub_tile_tbl) %>% dplyr::select(id_grid, RASTER_VALUE = countries), by = c("id_grid" = "id_grid")) %>% 
            dplyr::left_join(country_codes, by = c("RASTER_VALUE" = "RASTER_VALUE")) %>%
	    readr::write_csv(fname_forest_ts)
          #  dplyr::left_join(tibble::as_tibble(sub_tile_tbl) %>% dplyr::select(id, countries), by = c("id" = "id")) %>% 
          #  readr::write_csv(path = fname_forest_ts)

        } 
      }
      
    }
    
    # --------------------------------------------------------------------------------------
    # calculate total forest area 
    forest <- forest_timeseries %>% 
      dplyr::filter(year %in% c(2000, 2010, 2019)) %>% 
      dplyr::group_by(id) %>% 
      dplyr::summarise(area_forest_2000 = area_forest[year == 2000], 
                       area_accumulated_forest_loss_2010 = accumulated_loss[year == 2010],
                       area_accumulated_forest_loss_2019 = accumulated_loss[year == 2019],
		       .groups = 'drop')
    
    out_sub_tile_tbl <- sub_tile_tbl %>% 
      dplyr::left_join(forest, by = c("id" = "id")) %>% 
      dplyr::left_join(forest_mine, by = c("id_grid" = "id_grid")) %>% 
      tidyr::replace_na(list(area_forest_2000_mine_lease = 0,
			     area_accumulated_loss_mine_lease_2010 = 0,
			     area_accumulated_loss_mine_lease_2019 = 0,
			     area_mine = 0))
    
    # --------------------------------------------------------------------------------------
    # replace local id with global id_grid build raster output 
    out_forest_timeseries <- sub_tile_tbl %>% 
      tibble::as_tibble() %>% 
      dplyr::select(id_grid) %>% 
      dplyr::left_join(out_forest_timeseries, by = c("id_grid" = "id_grid")) %>%
      tidyr::complete(year = tidyr::full_seq(2000:2019, 1), nesting(id_grid), fill = list(area_forest_loss_mine_lease = 0, area_forest_mine_lease = 0)) %>%
      dplyr::select(-area_forest_mine_lease) %>%
      tidyr::spread(year, area_forest_loss_mine_lease) %>% 
      dplyr::select(-id_grid) %>% 
      as.matrix()
      #  dplyr::select(-id, id_grid, area_loss, countries) %>% 
      #  dplyr::left_join(mine_forest_loss_time_series, by = c("id_grid" = "id_grid", "year" = "year")) %>% 
      #  tidyr::replace_na(list(area_forest_mine_lease = 0, accumulated_loss_mine_lease = 0)) %>%
      #  dplyr::group_by(countries, year) %>%
      #  dplyr::summarise(area_forest_mine_lease = sum(area_forest_mine_lease, na.rm = TRUE), accumulated_loss_mine_lease = sum(accumulated_loss_mine_leas, na.rm = TRUE), .groups = 'drop')

    # --------------------------------------------------------------------------------------
    # write results to file 
    #readr::write_csv(out_forest_timeseries, path = fname_forest_ts)
    out_sub_tile_tbl %>% 
      dplyr::select(-id) %>% 
      sf::st_write(dsn = fname_grid_sf, factorsAsCharacter = TRUE, delete_dsn = TRUE, quiet = TRUE)
    
    r_out <- out_sub_tile_tbl %>% 
      dplyr::transmute(id = id,
                       area_forest_2000 = as.numeric(area_forest_2000), 
                       area_accumulated_forest_loss_2010 = as.numeric(area_accumulated_forest_loss_2010), 
                       area_accumulated_forest_loss_2019 = as.numeric(area_accumulated_forest_loss_2019), 
                       area_forest_2000_mine_lease = as.numeric(area_forest_2000_mine_lease), 
                       area_accumulated_loss_mine_lease_2010 = as.numeric(area_accumulated_loss_mine_lease_2010), 
                       area_accumulated_loss_mine_lease_2019 = as.numeric(area_accumulated_loss_mine_lease_2019), 
                       area_mine = as.numeric(area_mine)) %>% 
      tibble::as_tibble() %>% 
      dplyr::select(-geometry)
    
    r_out <- r_out %>% 
      tidyr::complete(id = tidyr::full_seq(1:ncell(sub_tile_grid), 1), fill = list(area_forest_2000 = 0, 
                                                                                   area_accumulated_forest_loss_2010 = 0, 
                                                                                   area_accumulated_forest_loss_2019 = 0, 
                                                                                   area_forest_2000_mine_lease = 0, 
                                                                                   area_accumulated_loss_mine_lease_2010 = 0, 
                                                                                   area_accumulated_loss_mine_lease_2019 = 0, 
                                                                                   area_mine = 0))

 
    r_forest_area <- raster::writeValues(r_forest_area, r_out$area_forest_2000, start = row_start)
    r_forest_area_loss_2010 <- raster::writeValues(r_forest_area_loss_2010, r_out$area_accumulated_forest_loss_2010, start = row_start)
    r_forest_area_loss_2019 <- raster::writeValues(r_forest_area_loss_2019, r_out$area_accumulated_forest_loss_2019, start = row_start)
    r_mine_lease_area <- raster::writeValues(r_mine_lease_area, r_out$area_mine, start = row_start)
    r_mine_lease_forest_2000 <- raster::writeValues(r_mine_lease_forest_2000, r_out$area_forest_2000_mine_lease, start = row_start)
    r_mine_lease_forest_loss_2010 <- raster::writeValues(r_mine_lease_forest_loss_2010, r_out$area_accumulated_loss_mine_lease_2010, start = row_start)
    r_mine_lease_forest_loss_2019 <- raster::writeValues(r_mine_lease_forest_loss_2019, r_out$area_accumulated_loss_mine_lease_2019, start = row_start)
    #mine_forest_loss_time_series_mat <- mine_forest_loss_time_series
    #dplyr::select(df, -forest) %>% tidyr::spread(year, loss) %>% dplyr::select(-id) %>% as.atrix()
    r_mine_lease_forest_loss_timeseries <- raster::writeValues(r_mine_lease_forest_loss_timeseries, out_forest_timeseries, start = row_start)

    # cat(paste0("\nTile ", id_hansen, " done! ", capture.output(Sys.time() - start_time)), file = log_file, append = TRUE)
    cat(paste0("\nTile ", id_hansen, " subtile ", b, " done! ", capture.output(Sys.time() - start_time)))
    
    return(TRUE)
    
  })
  
  r_forest_area <- raster::writeStop(r_forest_area)
  r_forest_area_loss_2010 <- raster::writeStop(r_forest_area_loss_2010)
  r_forest_area_loss_2019 <- raster::writeStop(r_forest_area_loss_2019)
  r_mine_lease_area <- raster::writeStop(r_mine_lease_area)
  r_mine_lease_forest_2000 <- raster::writeStop(r_mine_lease_forest_2000)
  r_mine_lease_forest_loss_2010 <- raster::writeStop(r_mine_lease_forest_loss_2010)
  r_mine_lease_forest_loss_2019 <- raster::writeStop(r_mine_lease_forest_loss_2019)
  r_mine_lease_forest_loss_timeseries <- raster::writeStop(r_mine_lease_forest_loss_timeseries)
  
  cat(paste0("\nTile ", id_hansen, " done! ", capture.output(Sys.time() - tile_start_time)))
  return(TRUE)
  
}


