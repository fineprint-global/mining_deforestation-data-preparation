library(tidyverse)
library(gdalUtils)
library(raster)
library(magrittr)
library(sf)
library(parallel)
library(fasterize)
source("./R/resample_gdal.R")
source("./R/parllel_land_mask.R")
source("./R/proxymity_gdal.R")

# --------------------------------------------------------------------------------------
# raster package setting 
raster::rasterOptions(tmpdir = "./raster_tmp/")
raster::rasterOptions(format = "GTiff")
raster::rasterOptions(progress = "text")
raster::beginCluster()

# --------------------------------------------------------------------------------------
# set path to data sets 
if(!exists("data_path"))
  data_path <- path.expand("/mnt/nfs_fineprint/data/geoserver")

# --------------------------------------------------------------------------------------
# set output file 
fineprint_grid_30sec_path <- path.expand(paste0(data_path, "/fineprint_grid_30sec"))
dir.create(fineprint_grid_30sec_path, showWarnings = FALSE, recursive = TRUE)

# --------------------------------------------------------------------------------------
# source data files 
src_pop_2000 <- paste0(data_path, "/rstudio/sedac-2019/population_density/2000/v4-11-05/gpw_v4_population_density_rev11_2000_30_sec.tif")
src_ecoregions <- paste0(data_path, "/ecoregions/Ecoregions2017.shp")
src_protected_area <- paste0(data_path, "/protected_areas/WDPA_Feb2019-shapefile-polygons.shp")
src_soilgrid <- paste0(data_path, "/rstudio/soilgrids-2019/soil_classification_250m_taxnwrb/2017/dl_2019-05/soil_classification_250m_TAXNWRB.tif")
src_esa_cci_2000 <- paste0(data_path, "/rstudio/esa-cci/land-cover/2000/v2-0-7/esacci_lc_2000_v2-0-7.tif")
src_elevation <- paste0(data_path, "/rstudio/amatulli-etal/topographic_variables/2018/dl_2019-07/elevation_1KMmn_GMTEDmn.tif")
src_slope <- paste0(data_path, "/rstudio/amatulli-etal/topographic_variables/2018/dl_2019-07/slope_1KMmn_GMTEDmn.tif")
src_mines <- paste0(data_path, "/mine_polygons/mine_polygons_world_v1r3.geojson")
src_accessibility_to_cities_2015 <- paste0(data_path, "/rstudio/weiss-etal/accessibility_to_cities/2015/v1-0/accessibility_to_cities_2015_v1-0.tif")
src_countries <- path.expand(paste0(data_path, "/country_eurostat/2016/dl_2019_06_20/countries_polygon.geojson"))

# --------------------------------------------------------------------------------------
# destination data files 
dst_land_mask <- paste0(fineprint_grid_30sec_path, "/template_land_mask_30sec.tif")
dst_ecoregions <- paste0(fineprint_grid_30sec_path, "/ecoregions_2017.tif")
dst_protected_areas <- paste0(fineprint_grid_30sec_path, "/distance_protected_areas.tif")
dst_pop_2000 <- paste0(fineprint_grid_30sec_path, "/population_density_2000.tif")
dst_soilgrid <- paste0(fineprint_grid_30sec_path, "/soilgrid.tif")
dst_esa_cci_2000 <- paste0(fineprint_grid_30sec_path, "/esa_cci_2000.tif")
dst_elevation <- paste0(fineprint_grid_30sec_path, "/elevation.tif")
dst_slope <- paste0(fineprint_grid_30sec_path, "/slope.tif")
dst_mines <- paste0(fineprint_grid_30sec_path, "/distance_mine.tif")
dst_accessibility_to_cities_2015 <- paste0(fineprint_grid_30sec_path, "/accessibility_to_cities_2015.tif")
dst_countries <- paste0(fineprint_grid_30sec_path, "/countries.tif")
dst_contiguous_land <- paste0(fineprint_grid_30sec_path, "/contiguous_land.tif")
dst_countries_concordance <- paste0(fineprint_grid_30sec_path, "/countries_concordance.csv")

# --------------------------------------------------------------------------------------
# get 30sec grid template parameter from gpw population and ecoregions 
grid_30sec <- raster::raster(src_pop_2000) %>% 
  raster::raster()

if(!file.exists(dst_land_mask)){
  
  # rasterize ecoregions  
  system.time(land_mask_grid_30sec <- sf::st_read(dsn = src_ecoregions, query = "SELECT ECO_ID FROM Ecoregions2017") %>% 
                fasterize::fasterize(raster = grid_30sec, field = NULL, fun = "last", background = NA))
  
  # write land mask to file 
  system.time(raster::writeRaster(land_mask_grid_30sec, filename = dst_land_mask, 
                                  options = c("COMPRESS=LZW", "TILED=YES"), overwrite = TRUE))
  
}
land_mask_grid_30sec <- raster::stack(dst_land_mask)

# --------------------------------------------------------------------------------------
# build 30sec grid for ecoregions 
if(!file.exists(dst_ecoregions)){
  
  # rasterize ecoregions  
  system.time(ecoregions <- sf::st_read(dsn = src_ecoregions, query = "SELECT ECO_ID FROM Ecoregions2017") %>% 
                fasterize::fasterize(raster = grid_30sec, field = "ECO_ID", fun = "last", background = NA))
  
  # write ecoregions to file 
  system.time(raster::writeRaster(ecoregions, filename = dst_ecoregions, 
                                  options = c("COMPRESS=LZW", "TILED=YES"), overwrite = TRUE))
  
}
# raster::raster(dst_ecoregions) %>% plot()


# --------------------------------------------------------------------------------------
# build 30sec grid for countries 
if(!file.exists(dst_countries)){
  
  # read country shape 
  countries <- sf::st_read(dsn = src_countries, query = "SELECT ISO3_CODE, COUNTRY_NAME_EN FROM countries_polygon")
  
  # create concordance table 
  countries_concordance <- countries %>% 
    sf::st_drop_geometry() %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(RASTER_VALUE = dplyr::row_number())
  
  readr::write_csv(countries_concordance, dst_countries_concordance)
  
  # save country raster values 
  tmp_countries <- "./raster_tmp/tmp_country_raster_values.gpkg"
  countries %>% 
    left_join(dplyr::select(countries_concordance, ISO3_CODE, RASTER_VALUE), by = c("ISO3_CODE" = "ISO3_CODE")) %>% 
    dplyr::select(RASTER_VALUE) %>% 
    sf::st_write(dsn = tmp_countries, delete_dsn = TRUE)
  
  # rasterize countries 
  system.time(write_fasterize(sf = tmp_countries, raster = grid_30sec, filename = dst_countries, 
                              field = "RASTER_VALUE", fun = "last", background = NA, overwrite = TRUE))
  
}
# raster::raster(dst_countries) %>% plot()

# --------------------------------------------------------------------------------------
# build 30sec grid for contiguous land 
if(!file.exists(dst_contiguous_land)){
  
  # read country shape 
  contiguous_land <- sf::st_read(dsn = src_countries, query = "SELECT ISO3_CODE FROM countries_polygon") %>% 
    sf::st_union() %>% 
    sf::st_cast("POLYGON")  %>% 
    sf::st_as_sf() %>% 
    dplyr::mutate(RASTER_VALUE = dplyr::row_number())
  
  # rasterize countries 
  system.time(write_fasterize(sf = countries, raster = grid_30sec, filename = dst_contiguous_land, 
                              field = "RASTER_VALUE", fun = "last", background = NA, overwrite = TRUE))
  
}
# raster::raster(dst_countries) %>% plot()


# --------------------------------------------------------------------------------------
# population density 
if(!file.exists(dst_pop_2000)){
  parllel_land_mask(x = raster::stack(list(src_pop_2000, land_mask_grid_30sec)),
                    dst_file = dst_pop_2000, co = c("COMPRESS=LZW", "TILED=YES"), 
                    overwrite = TRUE, verbose = TRUE)
}
# raster::raster(dst_pop_2000) %>% plot()

# --------------------------------------------------------------------------------------
# resample soilgrid to 30sec 
if(!file.exists(dst_soilgrid)){
  resample_gdal(src_file = src_soilgrid,
                dst_file = dst_soilgrid, 
                land_mask = dst_land_mask, 
                r = "mode", co = list("compress=LZW", "TILED=YES"),
                overwrite = TRUE, verbose = TRUE) 
}
# raster::raster(dst_soilgrid) %>% plot()

# --------------------------------------------------------------------------------------
# resample esa_cci_2000 to 30sec 
if(!file.exists(dst_esa_cci_2000)){
  resample_gdal(src_file = src_esa_cci_2000,
                dst_file = dst_esa_cci_2000, 
                land_mask = dst_land_mask, 
                r = "mode", co = list("compress=LZW", "TILED=YES"),
                overwrite = TRUE, verbose = TRUE) 
}
# raster::raster(dst_esa_cci_2000) %>% plot()

# --------------------------------------------------------------------------------------
# resample elevation to 30sec 
if(!file.exists(dst_elevation)){
  resample_gdal(src_file = src_elevation,
                dst_file = dst_elevation, 
                land_mask = dst_land_mask, 
                r = "bilinear", co = list("compress=LZW", "TILED=YES"),
                overwrite = TRUE, verbose = TRUE) 
}
# raster::raster(dst_elevation) %>% plot()

# --------------------------------------------------------------------------------------
# resample slope to 30sec 
if(!file.exists(dst_slope)){
  resample_gdal(src_file = src_slope,
                dst_file = dst_slope, 
                land_mask = dst_land_mask, 
                r = "bilinear", co = list("compress=LZW", "TILED=YES"),
                overwrite = TRUE, verbose = TRUE) 
}
# raster::raster(dst_slope) %>% plot()

# --------------------------------------------------------------------------------------
# build 30sec grid for distance to protected areas 
if(!file.exists(dst_protected_areas)){
  proxymity_gdal(src_file = src_protected_area, dst_file = dst_protected_areas, 
                 land_mask = land_mask_grid_30sec, field = NULL, background = NA, fun = "last",
                 co = list("compress=LZW", "TILED=YES"), verbose = TRUE, overwrite = TRUE)
}
# raster::raster(dst_protected_areas) %>% plot()

# --------------------------------------------------------------------------------------
# build 30sec grid for distance to mines 
if(!file.exists(dst_mines)){
  proxymity_gdal(src_file = src_mines, dst_file = dst_mines, 
                 land_mask = land_mask_grid_30sec, field = NULL, background = NA, fun = "last",
                 co = list("compress=LZW", "TILED=YES"), verbose = TRUE, overwrite = TRUE)
}
# raster::raster(dst_mines) %>% plot()

# --------------------------------------------------------------------------------------
# resample elevation to 30sec 
if(!file.exists(dst_accessibility_to_cities_2015)){
  resample_gdal(src_file = src_accessibility_to_cities_2015,
                dst_file = dst_accessibility_to_cities_2015, 
                land_mask = dst_land_mask, 
                r = "bilinear", co = list("compress=LZW", "TILED=YES"),
                overwrite = TRUE, verbose = TRUE) 
}
# raster::raster(dst_accessibility_to_cities_2015) %>% plot()


# --------------------------------------------------------------------------------------
# clean tmp folder 
raster::endCluster()

