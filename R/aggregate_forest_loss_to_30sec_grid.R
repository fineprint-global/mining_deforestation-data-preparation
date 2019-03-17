aggregate_forest_loss_to_30sec_grid <- function(area, year, grid_file, id_hansen, id_gtopo, sf_list, output_path, log_file, ncores){
  
  # area <- tiles_aggregated_forest_loss$area[10]
  # year <- tiles_aggregated_forest_loss$year[10]
  # grid_file <- tiles_aggregated_forest_loss$grid_file[10]
  # id_hansen <- tiles_aggregated_forest_loss$id_hansen[10]
  # id_gtopo <- tiles_aggregated_forest_loss$id_gtopo[10]
  # output_path <- "/mnt/nfs_fineprint/tmp/grid_30sec/deforestation"
  
  if(is.null(log_file))
    log_file <- date() %>% 
    stringr::str_replace_all(" ", "_") %>% 
    str_replace_all(":", "") %>% 
    stringr::str_glue(".log") 
  
  dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
  
  # ead tile 
  tile <- raster::stack(list(area = area, year = year))
  
  # Split processing blocks
  r_blocks <- raster::blockSize(tile)
  
  # Parallel processing 
  start_time <- Sys.time()
  cat(paste0("Processing forest loss tile ", id_hansen, " using ", ncores," cores"), file = log_file, append = TRUE)
  block_values <- parallel::mclapply(1:r_blocks$n, mc.cores = ncores, function(b){
    
    cat(paste0("\nProcessing block ", b, "/", r_blocks$n,""), file = log_file, append = TRUE)
    sub_tile_extent <- raster::extent(tile, r_blocks$row[b], r_blocks$row[b] + r_blocks$nrows[b] - 1, 1, ncol(tile))
    sub_tile_bbox <- sf::st_bbox(sub_tile_extent) %>% 
      sf::st_as_sfc() %>% 
      sf::st_sfc(crs = sf::st_crs(tile))
    
    q_string <- sub_tile_bbox %>% 
      sf::st_as_text() %>% 
      stringr::str_glue("SELECT * FROM ",id_gtopo," WHERE ST_Covers(ST_GeomFromText('",.,"'), geom);")
    
    sub_tile_grid <- sf::st_read(dsn = grid_file, query = q_string) 
    
    if(nrow(sub_tile_grid) < 1){
      return(NULL)
    }
    
    sub_tile_grid <- dplyr::mutate(sub_tile_grid, id = as.integer(rownames(sub_tile_grid)))
    sub_tile <- raster::crop(tile, y = sub_tile_grid)
    
    velox_tile <- raster::stack(c(sub_tile, lapply(sf_list, fasterize::fasterize, raster = raster::raster(sub_tile), field = "attr", fun = "last"))) %>%
      velox::velox()
    
    r_values <- velox_tile$extract(sp = sub_tile_grid, df = TRUE) %>% 
      tibble::as_tibble() %>% 
      dplyr::rename_all(dplyr::funs(make.names(c("id", names(tile), names(sf_list))))) %>%
      dplyr::filter(!is.na(ecoregion), !is.na(protected)) %>%
      dplyr::left_join(sub_tile_grid, by = c("id" = "id")) %>% 
      dplyr::mutate(id_grid = as.character(id_grid), 
                    year = as.integer(year), 
                    ecoregion = as.integer(ecoregion),
                    protected = as.integer(protected),
                    elevation = as.numeric(elevation),
                    area = as.numeric(area)) %>% 
      dplyr::group_by(id_grid, year, ecoregion, protected) %>%
      dplyr::summarise(elevation = mean(elevation, na.rm = TRUE), area = sum(area, na.rm = TRUE)) %>% 
      dplyr::ungroup()
    
    return(r_values)
    
  })
  gc(verbose = FALSE)
  tile_fname <- paste0(output_path, "/", id_gtopo, "_", id_hansen, ".csv")
  readr::write_csv(dplyr::bind_rows(block_values), path = tile_fname, append = FALSE)
  end_time <- Sys.time()
  cat(paste0("\nTile ", id_hansen, " done! ", capture.output(end_time - start_time)), file = log_file, append = TRUE)
  
  return(tile_fname)
}
