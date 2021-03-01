library(tidyverse)
library(parallel)

if(!exists("data_path"))
  data_path <- "/mnt/nfs_fineprint/data/geoserver"

fineprint_grid_30sec_path <- path.expand(paste0(data_path, "/fineprint_grid_30sec"))

output_path <- paste0(fineprint_grid_30sec_path, "/", grid_version)

files_path <- list.files(path = output_path, pattern = "csv$", recursive = TRUE,  full.names = TRUE, include.dirs = TRUE)

tbl_list <- parallel::mclapply(files_path, mc.cores = 10L, readr::read_csv) %>%
  dplyr::bind_rows() %>%
  dplyr::group_by(year, RASTER_VALUE, ISO3_CODE, COUNTRY_NAME_EN) %>% 
  dplyr::summarise(area_direct_forest_loss = sum(area_forest_loss_mine_lease, na.rm = TRUE), 
                   area_remaining_forest = sum(area_forest_mine_lease, na.rm = TRUE))

readr::write_csv(tbl_list, stringr::str_glue("{output_path}/mine_forest_timeseries.csv"))

