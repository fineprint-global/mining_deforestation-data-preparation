aggregate_forest_loss_to_30sec_grid <- function(area, year, treecover2000, id_hansen, id_gtopo, dem, sf_list, output_path, log_file = NULL, ncores){

  # area <- tiles_aggregated_forest_loss$area[1]
  # year <- tiles_aggregated_forest_loss$year[1]
  # treecover2000 <- tiles_aggregated_forest_loss$treecover2000[1]
  # id_hansen <- tiles_aggregated_forest_loss$id_hansen[1]
  # id_gtopo <- tiles_aggregated_forest_loss$id_gtopo[1]
  # dem <- tiles_aggregated_forest_loss$dem[1]
  # output_path <- "~/tmp/grid_30sec/deforestation_TEST"
  
  # if(is.null(log_file))
  #   log_file <- date() %>% 
  #     stringr::str_replace_all(" ", "_") %>% 
  #     str_replace_all(":", "") %>% 
  #     stringr::str_glue(".log") 
  
  dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

  # ead tile 
  tile <- raster::stack(list(area = area, year = year, treecover2000 = treecover2000))
  dem_tile <- raster::stack(list(dem = dem))
  
  # Split processing blocks
  r_blocks <- raster::blockSize(tile, minrows = 160)
  
  # Parallel processing 
  start_time <- Sys.time()
  # cat(paste0("\nProcessing forest loss tile ", id_hansen, " using ", ncores," cores"), file = log_file, append = TRUE)
  cat(paste0("\nProcessing forest loss tile ", id_hansen, " using ", ncores," cores"))
  
#  block_values <- parallel::mclapply(1:r_blocks$n, mc.cores = ncores, function(b){
  block_values <- lapply(1:r_blocks$n, function(b){
    
    grid_fname <- paste0(output_path, "/", id_gtopo, "_", id_hansen, "_", b, ".shp")
    data_fname <- paste0(output_path, "/", id_gtopo, "_", id_hansen, "_", b, ".csv")

    if(file.exists(grid_fname) && file.exists(data_fname)){
      return(TRUE)
    }

    # cat(paste0("\nProcessing block ", b, "/", r_blocks$n,""), file = log_file, append = TRUE)
    cat(paste0("\nProcessing block ", b, "/", r_blocks$n,""))
    
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

    # Get sub tile data 
    sub_tile <- raster::crop(tile, sub_tile_extent)
    velox_tile <- raster::stack(c(sub_tile, lapply(sf_list, fasterize::fasterize, raster = raster::raster(sub_tile), field = "attr", fun = "last"))) %>% 
      velox::velox()

    # Aggregate values to 30sec grid 
    r_values <- velox_tile$extract(sp = sub_tile_grid, df = TRUE) %>% 
      tibble::as_tibble() %>% 
      dplyr::rename_all(list(~make.names(c("id", names(tile), names(sf_list))))) %>%
      dplyr::filter(!near(treecover2000, 0)) %>% 
      dplyr::left_join(sub_tile_grid, by = c("id" = "id")) %>%
      dplyr::transmute(id_grid = as.character(id_grid),
                       year = as.integer(year + 2000), 
                       ecoregion = as.integer(ecoregion),
                       ecoregion = ifelse(is.na(ecoregion), 9999, ecoregion),          # Ecoregions NA to 9999
                       protected = ifelse(as.integer(protected) <= year, TRUE, FALSE), # Check if deforestation happens after the status of protected
                       protected = !is.na(protected),
                       area = as.numeric(area) * (as.numeric(treecover2000) / 100)) %>% 
      dplyr::group_by(id_grid, year, ecoregion, protected) %>%
      dplyr::summarise(area = sum(area, na.rm = TRUE)) %>% 
      dplyr::ungroup()
    
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

  # connect PostGIS database ------------------------------------------------
  # conn <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
  #                       host = Sys.getenv("db_host"),
  #                       port = Sys.getenv("db_port"),
  #                       dbname = Sys.getenv("db_name"),
  #                       user = Sys.getenv("db_user"),
  #                       password = Sys.getenv("db_password"))
  
  # Insert grid to db  ------------------------------------------------------
  ## DBI::dbSendQuery(conn = conn, statement = paste0("DELETE FROM forest;"))
  ## DBI::dbSendQuery(conn = conn, statement = paste0("DELETE FROM grid;"))
  #do.call("rbind", lapply(block_values, function(x) x$grid)) %>% 
  #  sf::st_write(dsn = conn, layer = "grid", append = TRUE, factorsAsCharacter = TRUE)

  # Insert forest data to db  -----------------------------------------------
  #db_status <- dplyr::bind_rows(lapply(block_values, function(x) x$tbl)) %>% 
  #  DBI::dbWriteTable(conn = conn, name = "forest", append = TRUE, value = ., row.names = FALSE)
  
  # disconnect PostGIS database ---------------------------------------------
  #DBI::dbDisconnect(conn)
  #cat(paste0("\nTile ", id_hansen, " done with DB insert ", db_status, " ", capture.output(Sys.time() - start_time)), file = log_file, append = TRUE)
  #return(db_status)

  # ### pfusch - replace with DB connection  
  # grid_fname <- paste0(output_path, "/", id_gtopo, "_", id_hansen, ".shp")
  # data_fname <- paste0(output_path, "/", id_gtopo, "_", id_hansen, ".csv")
  # do.call("rbind", lapply(block_values, function(x) x$grid)) %>%
  #   sf::st_write(dsn = grid_fname, factorsAsCharacter = TRUE, delete_layer = TRUE)
  # 
  # dplyr::bind_rows(lapply(block_values, function(x) x$tbl)) %>%
  #   readr::write_csv(path = data_fname, append = FALSE)
  # 
  # cat(paste0("\nTile ", id_hansen, " done! ", capture.output(Sys.time() - start_time)), file = log_file, append = TRUE)
  cat(paste0("\nTile ", id_hansen, " done! ", capture.output(Sys.time() - start_time)))
  return(TRUE)
  
}
