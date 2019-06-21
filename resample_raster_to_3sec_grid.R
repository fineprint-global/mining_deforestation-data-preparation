library(tidyverse)
library(magrittr)
library(raster)
raster::rasterOptions(tmpdir = "./raster_tmp/")

# --------------------------------------------------------------------------------------
# set path to data sets 
if(!exists("data_path"))
  data_path <- "/mnt/nfs_fineprint/data/geoserver"

# --------------------------------------------------------------------------------------
# set output file 
fineprint_grid_30sec_path <- path.expand(paste0(data_path, "/fineprint_grid_30sec"))
dir.create(fineprint_grid_30sec_path, showWarnings = FALSE, recursive = TRUE)

# --------------------------------------------------------------------------------------
# get 30sec grid template 
template_30sec_grid <- path.expand(paste0(data_path, "/rstudio/sedac-2019/population_density/2000/v4-11-05/gpw_v4_population_density_rev11_2000_30_sec.tif")) %>% 
  raster::brick() %>% 
  raster::brick(nl = 1)

# # --------------------------------------------------------------------------------------
# # elevation 
# elevation <- path.expand(paste0(data_path, "/rstudio/amatulli-etal/topographic_variables/2018/dl_2019-07/elevation_1KMmn_GMTEDmn.tif")) %>% 
#   raster::brick()
# names(elevation) <- "elevation"
# raster::writeRaster(x = elevation, filename = paste0(fineprint_grid_30sec_path, "/elevation.tif"))
# 
# # --------------------------------------------------------------------------------------
# # slope 
# slope <- path.expand(paste0(data_path, "/rstudio/amatulli-etal/topographic_variables/2018/dl_2019-07/slope_1KMmn_GMTEDmn.tif")) %>% 
#   raster::brick()
# names(slope) <- "slope"
# raster::writeRaster(x = slope, filename = paste0(fineprint_grid_30sec_path, "/slope.tif"))

# --------------------------------------------------------------------------------------
# soilgrid 
soilgrid <- path.expand(paste0(data_path, "/rstudio/soilgrids-2019/soil_classification_250m_taxnwrb/2017/dl_2019-05/soil_classification_250m_TAXNWRB.tif")) %>% 
  raster::brick()
names(soilgrid) <- "soilgrid"
soilgrid <- raster::resample(x = soilgrid, y = template_30sec_grid, method = "ngb", 
                             filename = paste0(fineprint_grid_30sec_path, "/soilgrid.tif"))

# --------------------------------------------------------------------------------------
# esa_cci_2000 
esa_cci_2000 <- path.expand(paste0(data_path, "/rstudio/esa-cci/land-cover/2000/v2-0-7/esacci_lc_2000_v2-0-7.tif")) %>% 
  raster::brick()
names(esa_cci_2000) <- "esa_cci_2000"
esa_cci_2000 <- raster::resample(x = esa_cci_2000, y = template_30sec_grid, method = "ngb", 
                                 filename = paste0(fineprint_grid_30sec_path, "/esa_cci_2000.tif"))

# --------------------------------------------------------------------------------------
# pop_2000
pop_2000 = path.expand(paste0(data_path, "/rstudio/sedac-2019/population_density/2000/v4-11-05/gpw_v4_population_density_rev11_2000_30_sec.tif")) %>% 
  raster::brick()
names(pop_2000) <- "pop_2000"
pop_2000 <- raster::resample(x = pop_2000, y = template_30sec_grid, method = "ngb", 
                             filename = paste0(fineprint_grid_30sec_path, "/pop_2000.tif"))

