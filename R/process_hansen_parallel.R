process_hansen_parallel <- function(x, y, sf_list, r_tbl, output_path, ncores, log_file = NULL){
  
  if(is.null(log_file))
    log_file <- date() %>% 
      stringr::str_replace_all(" ", "_") %>% 
      str_replace_all(":", "") %>% 
      stringr::str_glue(".log") 
  
  dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
  
  # 0. Make 30 arc-sec grid from GTOPO30
  sf_fpath <- paste0(output_path, "/", y, ".gpkg") 
  if(file.exists(sf_fpath)){
    # TODO: REPLACE WITH POSTGIS CONNECTION 
    start_time <- Sys.time()
    cat(paste0("Reading grid ", y, " from ", sf_fpath, " "), file = log_file, append = TRUE)
    tile_grid <- sf::st_read(dsn = sf_fpath, quiet = TRUE)
    end_time <- Sys.time()
    cat(paste0(capture.output(end_time - start_time), "\n"), file = log_file, append = TRUE)
  } else {
    start_time <- Sys.time()
    cat(paste0("Creating grid", y, "at", sf_fpath, " "), file = log_file, append = TRUE)
    fname <- basename(x)
    tile_grid <- stars::read_stars(x) %>% 
      sf::st_as_sf(x, merge = FALSE, as_points = FALSE) %>% 
      dplyr::rename(elevation:=!!fname) %>% 
      tibble::rowid_to_column(var = "id_grid") %>% 
      dplyr::mutate(id_grid = stringr::str_pad(string = id_grid, width = 8, pad = "0")) %>% 
      dplyr::mutate(id_grid = paste0(y, "G", id_grid)) 
    sf::st_write(obj = tile_grid, dsn = sf_fpath, quiet = TRUE)
    end_time <- Sys.time()
    cat(paste0(capture.output(end_time - start_time), "\n"), file = log_file, append = TRUE)
  }
  
  # 2. Filter forest tiles overlapping GTOPO30 
  start_time <- Sys.time()
  cat(paste0("Filtering forest loss tiles interseting grid ", y, " "), file = log_file, append = TRUE)
  tiles_subset <- r_tbl %>% 
    dplyr::slice(which(sapply(sf::st_intersects(r_tbl, tile_grid), function(i) length(i)>0))) %>% 
    dplyr::filter()
  end_time <- Sys.time()
  cat(paste0(capture.output(end_time - start_time), "\n"), file = log_file, append = TRUE)
  
  # 3. For each forest tile extract attrubutes and inset to database 
  grid_files <- sapply(seq_along(tiles_subset$r), function(tl){
    tile <- tiles_subset$r[[tl]]
    start_time <- Sys.time()
    cat(paste0("Processing forest loss tile ", tl, "/", length(tiles_subset$r)," in parallel"), file = log_file, append = TRUE)
    r_blocks <- raster::blockSize(tile)
    
    # Process by block
    block_values <- parallel::mclapply(1:r_blocks$n, mc.cores = ncores, function(b){
      gc(verbose = FALSE)
      cat(paste0("\nProcessing block ", b, "/", r_blocks$n,""), file = log_file, append = TRUE)
      
      # cat(paste0("\n\tChecking for overlapping grid cells ..."), file = log_file, append = TRUE)
      sub_tile_extent <- raster::extent(tile, r_blocks$row[b], r_blocks$row[b] + r_blocks$nrows[b] - 1, 1, ncol(tile))
      sub_tile_bbox <- sf::st_bbox(sub_tile_extent) %>% 
        sf::st_as_sfc() %>% 
        sf::st_sfc(crs = sf::st_crs(tile))
      idx <- sf::st_intersects(sub_tile_bbox, tile_grid)
      if(length(idx[[1]]) < 1){
        return(NULL)
      }
      # cat(paste0("Done "), file = log_file, append = TRUE)
      
      # cat(paste0("\n\tFiltering intersecting cells ..."), file = log_file, append = TRUE)
      subset_tile_grid <- tile_grid %>% 
        dplyr::slice(idx[[1]]) %>% 
        dplyr::filter() %>% 
        dplyr::mutate(id = 1:n(), id_grid = paste0(y, "G", idx[[1]]))
      # cat(paste0("Done "), file = log_file, append = TRUE)
      
      # cat(paste0("\n\tCroping raster ..."), file = log_file, append = TRUE)
      sub_tile <- raster::crop(tile, y = subset_tile_grid)
      # cat(paste0("Done "), file = log_file, append = TRUE)
      
      # cat(paste0("\n\tRasterizing vector data and creating velox object ..."), file = log_file, append = TRUE)
      velox_tile <- raster::stack(c(sub_tile, lapply(sf_list, fasterize::fasterize, raster = raster::raster(sub_tile), field = "attr", fun = "last"))) %>% 
        velox::velox()  
      # cat(paste0("Done "), file = log_file, append = TRUE)
      
      # cat(paste0("\n\tExtracting and tidying data from raster ..."), file = log_file, append = TRUE)
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
      # cat(paste0("Done "), file = log_file, append = TRUE)
      
      return(r_values)
      
    })
    tile_fname <- paste0(output_path, "/", y, "_", tiles_subset$id_tile[tl], ".csv")
    readr::write_csv(dplyr::bind_rows(block_values), path = tile_fname, append = FALSE)
    end_time <- Sys.time()
    cat(paste0("\nTile ", tl, " done! ", capture.output(end_time - start_time)), file = log_file, append = TRUE)
    return(tile_fname)
  })
  
  return(grid_files)
  
}

