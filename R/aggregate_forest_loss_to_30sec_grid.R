aggregate_forest_loss_to_30sec_grid <- function(area, year, id_hansen, id_gtopo, dem, sf_list, output_path, log_file, ncores){

  # area <- tiles_aggregated_forest_loss$area[1]
  # year <- tiles_aggregated_forest_loss$year[1]
  # treecover2000 <- tiles_aggregated_forest_loss$treecover2000[1]
  # id_hansen <- tiles_aggregated_forest_loss$id_hansen[1]
  # id_gtopo <- tiles_aggregated_forest_loss$id_gtopo[1]
  # dem <- tiles_aggregated_forest_loss$dem[1]
  # output_path <- "~/tmp/grid_30sec/deforestation_TEST"
  
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
      dplyr::mutate(block_id = b) 

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
      # dplyr::filter(!near(treecover2000, 0)) %>% 
      dplyr::mutate(year = as.integer(year) + 2000, 
                    ecoregion = as.integer(ecoregion),
                    ecoregion = ifelse(is.na(ecoregion), 9999, ecoregion),          # Ecoregions NA to 9999
                    protected = ifelse(as.integer(protected) <= year, TRUE, FALSE), # Check if deforestation happens after the status of protected
                    protected = !is.na(protected),
                    area = as.numeric(area) * (as.numeric(treecover2000) / 100)) %>% 
      dplyr::group_by(id, year, ecoregion, protected) %>%
      dplyr::summarise(area = sum(area, na.rm = TRUE)) %>% 
      dplyr::ungroup()
    
    # Calculate forest cover and forest loss time series 
    forest_loss_area <- r_values %>% 
      dplyr::filter(year == 2000) %>% 
      dplyr::rename(area_2000 = area) %>% 
      dplyr::select(-year) %>% 
      dplyr::right_join(r_values, by = c("id" = "id", "ecoregion" = "ecoregion", "protected" = "protected")) %>%  
      dplyr::rename(area_loss = area) %>% 
      dplyr::mutate(area_loss = ifelse(year == 2000, 0, area_loss)) %>% 
      dplyr::group_by(id, ecoregion, protected) %>% 
      dplyr::arrange(id, year, ecoregion, protected) %>% 
      dplyr::mutate(accumulated_loss = cumsum(area_loss)) %>% 
      dplyr::mutate(area_forest = area_2000 - accumulated_loss) %>% 
      dplyr::select(-area_2000) %>% 
      dplyr::mutate(block_id = b)

    return(list(grid = sub_tile_grid, tbl = forest_loss_area))
    
  })
  
  # connect PostGIS database ------------------------------------------------
  Sys.setenv(PGPASSFILE=".pgpass")
  conn <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         host = Sys.getenv("db_host"),
                         port = Sys.getenv("db_port"),
                         dbname = Sys.getenv("db_name"),
                         user = "postgres")
  
  # clear grid table  --------------------------------------------------
  ## DBI::dbSendQuery(conn = conn, statement = paste0("DELETE FROM grid;"))
  
  do.call("rbind", lapply(block_values, function(x) x$geomtry)) %>% 
    dplyr::mutate(id_gtopo, id_hansen) %>% 
    dplyr::rename(id_grid = id, id_block = block_id) %>% 
    sf::st_write(dsn = conn, layer = "grid", append = TRUE, factorsAsCharacter = TRUE)
  
  # TODO: Return id fk from database(query over id_gtopo, id_hansen)
  fk_grid_id <- sf::st_read(dsn = conn, layer = "grid") %>% 
    dplyr::filter(id_gtopo == id_gtopo, id_hansen == id_hansen)
  
  # JOIN BY id_block AND id_grid TO GET THE FK FOR FOREST TABLE 
  dplyr::bind_rows(lapply(block_values, function(x) x$tbl))
  fk_grid_id
  
  
  # TODO: Insset data to forest table 
  gc(verbose = FALSE)
  tile_fname <- paste0(output_path, "/", id_gtopo, "_", id_hansen, ".csv")
  readr::write_csv(dplyr::bind_rows(block_values), path = tile_fname, append = FALSE)
  end_time <- Sys.time()
  cat(paste0("\nTile ", id_hansen, " done! ", capture.output(end_time - start_time)), file = log_file, append = TRUE)

  return(tile_fname)
  
}
