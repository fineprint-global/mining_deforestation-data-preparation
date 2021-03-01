library(tidyverse)
library(raster)
library(gdalUtils)
raster::rasterOptions(tmpdir = "./raster_tmp/")
raster::rasterOptions(format = "GTiff")

# --------------------------------------------------------------------------------------
# set path to data sets
data_path <- paste0(Sys.getenv("HOME"),"/data/geoserver")

# --------------------------------------------------------------------------------------
# get path dir data sets
pixel_area_dir <- path.expand(paste0(data_path, "/hansen/pixel_area_new"))
forest_loss_dir <- path.expand(paste0(data_path, "/hansen/v1.7/lossyear"))
treecover2000_dir <- path.expand(paste0(data_path, "/hansen/v1.7/treecover2000"))

# --------------------------------------------------------------------------------------
# create global VRT
pixel_area_vrt <- stringr::str_glue("{pixel_area_dir}/area.vrt")
gdalUtils::gdalbuildvrt(gdalfile = dir(pixel_area_dir, pattern = ".tif$", full.names = TRUE),
                        te = c(-180, -90, 180, 90),
                        output.vrt = pixel_area_vrt)

forest_loss_vrt <- stringr::str_glue("{forest_loss_dir}/lossyear.vrt")
gdalUtils::gdalbuildvrt(gdalfile = dir(forest_loss_dir, pattern = ".tif$", full.names = TRUE),
                        te = c(-180, -90, 180, 90),
                        output.vrt = forest_loss_vrt)

treecover2000_vrt <- stringr::str_glue("{treecover2000_dir}/treecover2000.vrt")
gdalUtils::gdalbuildvrt(gdalfile = dir(treecover2000_dir, pattern = ".tif$", full.names = TRUE),
                        te = c(-180, -90, 180, 90),
                        output.vrt = treecover2000_vrt)
