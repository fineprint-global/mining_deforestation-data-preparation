library(tidyverse)
library(RPostgreSQL)
library(DBI)
library(sf)
library(parallel)
readRenviron(".Renviron")

# Move data from wucluster 
# sync: rsync -avzh -e ssh vmaus@clusterwu:/gpfs/home/home/vmaus/data/forest_loss_30sec/* .
# check for changes: rsync -aunv -e ssh vmaus@clusterwu:/gpfs/home/home/vmaus/data/forest_loss_30sec/* .
# update files: rsync -avzh --update -e ssh vmaus@clusterwu:/gpfs/home/home/vmaus/data/forest_loss_30sec/* .
# new files only: rsync -avzh --ignore-existing -e ssh vmaus@clusterwu:/gpfs/home/home/vmaus/data/forest_loss_30sec/* .

log_file <- date() %>% 
  stringr::str_replace_all(" ", "_") %>% 
  str_replace_all(":", "") %>% 
  stringr::str_glue(".log") 

# Clean forest table --------------------------------------------------------
file_path <- dir("~/workspace/data/forest_loss_30sec", pattern = ".csv$", full.names = TRUE)

out <- parallel::mclapply(seq_along(file_path), mc.cores = 20, mc.preschedule = FALSE, function(i){

  f <- file_path[i]  
  cat(paste0(i, "/", length(file_path), " Inserting ",f," data to DB\n"))
  # connect PostGIS database --------------------------------------------------
  conn <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         host = Sys.getenv("db_host"),
                         port = Sys.getenv("db_port"),
                         dbname = Sys.getenv("db_name"),
                         user = Sys.getenv("db_user"),
                         password = Sys.getenv("db_password"))
  
  # read data from files ----------------------------------------------------
  cat("  Reading data from files... ")  
  forest_tbl <- readr::read_csv(f, col_types = c('ciliddd')) 
  grid_tbl <- stringr::str_replace(f, pattern = ".csv", replacement = ".shp") %>%
    sf::st_read(quiet = TRUE, stringsAsFactors = FALSE) 
  cat("Done\n")
  
  # DELETEME - Fixes id problem 
  new_id <- basename(f) %>%
    stringr::str_remove_all(".csv") %>%
    stringr::str_split(pattern = "_") %>%
    unlist()
  
  new_id <- paste0(new_id[1], new_id[2], stringr::str_pad(string = new_id[3], width = 4, pad = "0"), "B")
  
  concordance_id <- tibble(old_id = grid_tbl$id, new_id) %>% 
    tibble::rowid_to_column(var = "id_grid") %>% 
    dplyr::mutate(id_grid = stringr::str_pad(string = id_grid, width = 5, pad = "0")) %>% 
    dplyr::mutate(new_id = paste0(new_id, id_grid)) %>% 
    dplyr::select(-id_grid)
  
  forest_tbl <- forest_tbl %>% 
    dplyr::left_join(concordance_id, by = c("id_grid" = "old_id")) %>% 
    dplyr::select(-id_grid) %>% 
    dplyr::rename(id_grid = new_id)
  
  grid_tbl <- grid_tbl %>% 
    dplyr::left_join(concordance_id, by = c("id" = "old_id")) %>% 
    dplyr::select(id = new_id, elevation)
  ## DELETE TILL HERE   
  
  # Insert grid to db  ------------------------------------------------------
  cat("  Inserting grid geometry... ")
  db_log <- try(silent = TRUE, forest_tbl %>% 
    dplyr::filter(year == 2000) %>%  
    dplyr::select(id_grid, year, area_forest) %>% 
    dplyr::group_by(id_grid, year) %>% 
    summarise(area_forest = sum(area_forest, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(id = id_grid, area_forest_2000 = area_forest) %>% 
    dplyr::right_join(grid_tbl, by = c("id" = "id")) %>% 
    sf::st_write(dsn = conn, layer = "grid", append = TRUE, factorsAsCharacter = TRUE, quiet = TRUE))
  if(class(db_log)=="try-error"){
    cat(paste0(f, "\n", db_log), file = log_file, append = TRUE)
  } else {
    # Insert forest data to db  -----------------------------------------------
    cat("  Inserting forest data... ")
    db_log <- try(silent = TRUE, DBI::dbWriteTable(conn = conn, name = "forest", append = TRUE, value = forest_tbl, row.names = FALSE))
    if(class(db_log)=="try-error"){
      cat(paste0(f, "\n", db_log), file = log_file, append = TRUE)
    } else {
      cat(paste0(i, "/", length(file_path), " SUCCESS forest data inserted from ",f," to DB \n"), file = log_file, append = TRUE)
    }
  }

  # disconnect PostGIS database -----------------------------------------------
  DBI::dbDisconnect(conn)
  
  return(TRUE)
  
})

# connect PostGIS database --------------------------------------------------
conn <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                       host = Sys.getenv("db_host"),
                       port = Sys.getenv("db_port"),
                       dbname = Sys.getenv("db_name"),
                       user = Sys.getenv("db_user"),
                       password = Sys.getenv("db_password"))

# analysis databse ----------------------------------------------------------
cat("Analysing grid table\n", file = log_file, append = TRUE)
system.time(DBI::dbSendQuery(conn = conn, statement = "VACUUM ANALYZE grid;"))
cat("Analysing forest table\n", file = log_file, append = TRUE)
system.time(DBI::dbSendQuery(conn = conn, statement = "VACUUM ANALYZE forest;"))

# disconnect PostGIS database -----------------------------------------------
DBI::dbDisconnect(conn)

cat("\nDone!", file = log_file, append = TRUE)
