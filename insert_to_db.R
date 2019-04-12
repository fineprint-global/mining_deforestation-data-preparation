library(tidyverse)
library(RPostgreSQL)
library(DBI)
library(sf)
library(parallel)
readRenviron(".Renviron")

# Move data from wucluster 
# rsync -avzhe ssh --progress vmaus@clusterwu:/gpfs/home/home/vmaus/data/forest_loss_30sec .

log_file <- date() %>% 
  stringr::str_replace_all(" ", "_") %>% 
  str_replace_all(":", "") %>% 
  stringr::str_glue(".log") 

# Clean forest table --------------------------------------------------------
file_path <- dir("~/workspace/data/forest_loss_30sec", pattern = ".csv$", full.names = TRUE)

out <- parallel::mclapply(seq_along(file_path), mc.cores = 12, function(i){

  f <- file_path[i]  
  cat(paste0(i, "/", length(file_path), " Inserting ",f," data to DB\n"), file = log_file, append = TRUE)
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
  
  # Insert grid to db  ------------------------------------------------------
  cat("  Inserting grid geometry... ")
  forest_tbl %>% 
    dplyr::filter(year == 2000) %>%  
    dplyr::select(id_grid, year, area_forest) %>% 
    dplyr::group_by(id_grid, year) %>% 
    summarise(area_forest = sum(area_forest, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(id = id_grid, area_forest_2000 = area_forest) %>% 
    dplyr::right_join(grid_tbl, by = c("id" = "id")) %>% 
    sf::st_write(dsn = conn, layer = "grid", append = TRUE, factorsAsCharacter = TRUE, quiet = TRUE)
  cat("Done\n")
  
  # Insert forest data to db  -----------------------------------------------
  cat("  Inserting forest data... ")
  DBI::dbWriteTable(conn = conn, name = "forest", append = TRUE, value = forest_tbl, row.names = FALSE)
  cat("Done\n")

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
