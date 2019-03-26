create_30sec_grid <- function(x, y, output_path, ncores, log_file = NULL){
  
  require(sf)
  require(stars)
  require(tidyverse)
  
  if(is.null(log_file))
    log_file <- date() %>% 
      stringr::str_replace_all(" ", "_") %>% 
      str_replace_all(":", "") %>% 
      stringr::str_glue(".log") 
  
  dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
  
  # 0. Make grid from raster 
  sf_fpath <- paste0(output_path, "/", y, ".gpkg") 
  if(file.exists(sf_fpath)){
    cat(paste0("Grid ", y, " exists at ", sf_fpath, ". Nothing to do\n"), file = log_file, append = TRUE)
  } else {
    start_time <- Sys.time()
    cat(paste0("Creating grid ", y, " at ", sf_fpath, "\n"), file = log_file, append = TRUE)
    fname <- basename(x)
    tile_grid <- stars::read_stars(x) %>% 
      sf::st_as_sf(x, merge = FALSE, as_points = FALSE) %>% 
      dplyr::rename(elevation:=!!fname) %>% 
      tibble::rowid_to_column(var = "id_grid") %>% 
      dplyr::mutate(id_grid = stringr::str_pad(string = id_grid, width = 8, pad = "0")) %>% 
      dplyr::mutate(id_grid = paste0(y, "G", id_grid)) 
    sf::st_write(obj = tile_grid, dsn = sf_fpath, quiet = TRUE)
    end_time <- Sys.time()
    cat(paste0("\nGrid ", y, " created at ", sf_fpath, ". ", capture.output(end_time - start_time), "\n"), file = log_file, append = TRUE)
  }
  
  return(sf_fpath)
  
}

