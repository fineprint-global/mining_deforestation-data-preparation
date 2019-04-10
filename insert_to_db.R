library(tidyverse)
library(RPostgreSQL)
library(DBI)
library(sf)

# Move data from wucluster 
# rsync -avzhe ssh --progress vmaus@clusterwu:/gpfs/home/home/vmaus/data/forest_loss_30sec .

# connect PostGIS database ------------------------------------------------
conn <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                       host = Sys.getenv("db_host"),
                       port = Sys.getenv("db_port"),
                       dbname = Sys.getenv("db_name"),
                       user = Sys.getenv("db_user"),
                       password = Sys.getenv("db_password"))

#DBI::dbSendQuery(conn = conn, statement = paste0("DELETE FROM forest;"))

file_path <- dir("/mnt/nfs_fineprint/tmp/forest_loss_30sec/forest_loss_30sec", pattern = ".csv$", full.names = TRUE)

for (f in file_path[grep(pattern = "00N020E", x = file_path)]) {
  
  cat(paste0("Inserting ",f," data to DB\n"))
  
  # Insert grid to db  ------------------------------------------------------
  cat("Inserting geometry\n")
  stringr::str_replace(f, pattern = ".csv", replacement = ".shp") %>% 
    sf::st_read() %>% 
    sf::st_write(dsn = conn, layer = "grid", append = TRUE, factorsAsCharacter = TRUE)
  
  # Insert forest data to db  -----------------------------------------------
  cat("Inserting table")
  readr::read_csv(f) %>% 
    DBI::dbWriteTable(conn = conn, name = "forest", append = TRUE, value = ., row.names = FALSE)
  
}

# disconnect PostGIS database ---------------------------------------------
DBI::dbDisconnect(conn)

cat("\nDone!")
