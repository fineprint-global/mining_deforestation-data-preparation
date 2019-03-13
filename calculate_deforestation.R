library(tidyverse)
library(raster)
library(sf)
library(fasterize)
library(velox)
library(fineprintutils)

# list Hansen dataset
hansen_tiles <- dir("/mnt/nfs_fineprint/tmp/hansen/lossyear", pattern = ".tif$", full.name = TRUE) 
hansen_area_tiles <- dir("/mnt/nfs_fineprint/tmp/hansen_pixel_area", pattern = ".tif$", full.name = TRUE)

# Get echo regions 
# fineprintutils::download_file(url = "https://storage.googleapis.com/teow2016/Ecoregions2017.zip", destfile = "./input/ecoregions/Ecoregions2017.zip", mode = "wb")
# unzip(zipfile = "./input/ecoregions/Ecoregions2017.zip", exdir = "./input/ecoregions/")
ecoregions_sf <- sf::read_sf("./input/ecoregions/Ecoregions2017.shp")

# # Get protected areas 
# fineprintutils::download_file(url = "https://protectedplanet.net/downloads/WDPA_Feb2019?type=shapefile", destfile = "./input/protected_areas/WDPA_Feb2019.zip", mode = "wb")
# unzip(zipfile = "./input/protected_areas/WDPA_Feb2019.zip", exdir = "./input/protected_areas/")
# protected_areas_sf <- sf::read_sf("./input/protected_areas/WDPA_Feb2019-shapefile-polygons.shp")


area_dir <- "/mnt/nfs_fineprint/tmp/hansen_pixel_area"
dir.create(area_dir, showWarnings = FALSE, recursive = TRUE)
r_area_list <- parallel::mclapply(hansen_tiles, mc.cores = 4, function(f){
  
  output_filename <- paste0(area_dir, "/", basename(f))
  
  if(file.exists(output_filename)){
    return(output_filename)
  }
  
  print(cat("Read raster ", f))
  r <- raster::raster(f)
  
  r_intersects <- sf::st_bbox(r) %>% 
    sf::st_as_sfc() %>% 
    sf::st_intersects(ecoregions_sf, sparse = FALSE) %>% 
    any()
  
  if(!r_intersects){
    return(NULL)
  }

  print(cat("Calculate raster area ", f))
  ra <- raster::area(r, na.rm = FALSE, filename = output_filename, overwrite = TRUE)
  
  print(cat("Done ", f))
  return(output_filename)
  
})


f <- c(year = hansen_tiles[13], area = hansen_area_tiles[10])

# 0. Read hansen tile
r <- raster::stack(x = f)
r <- raster::addLayer(r, r[[1]])
names(r) <- c("year", "area", "ecoregions")
r_sub <- raster::crop(r, raster::extent(r, 38000, 40000, 1, 2000))

# 1. Make velox object 
r_velox_sub <- velox::velox(r_sub)

# 2. Rasterize eco PM
r_velox_sub$rasterize(spdf = ecoregions_sf, field = "ECO_ID", band = 3, background = -1)
# r_velox_sub$rasterize(spdf = protected_areas_sf, field = "WDPAID", band = 4, background = -1)
# r_aux <- r_velox_sub$as.RasterBrick()
# raster::plot(r_aux)

# 3. Read GTOPO grid
r_gtopo <- stars::read_stars("/mnt/nfs_fineprint/tmp/GTOPO30/gt30w060n40.tif")
r_grid <- sf::st_as_sf(r_gtopo, merge = FALSE, as_points = FALSE)
sf::write_sf(r_grid, dsn = "./wu_share/WU/Projekte/GRU/01_Projekte/11000518_EO4SC/02_Activities & Work packages/grid_test.shp")
# r_grid %>% sf::st_intersects(sf::st_sfc(sf::st_point(c(-59.93, -9.95)), crs = sf::st_crs(.)), .)
# r_grid %>% sf::st_intersects(sf::st_sfc(sf::st_point(c(-59.966, -8.204)), crs = sf::st_crs(.)), .)
sf_boost <- dplyr::slice(r_grid, c(1, 1936640, 2404949:2404950))
  # sf::st_geometry(r_grid) %>%  velox::boost()

tidy_values <- function(x){
  x %>% 
    tibble::as_tibble() %>% 
    dplyr::group_by(ID_sp, X1, X3) %>% 
    dplyr::summarise(area = sum(X2, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(id = ID_sp, year = X1, ECO_ID = X3, area)
}

r_values <- r_velox_sub$extract(sp = sf_boost, df = TRUE)

tidy_values(r_values) %>% 
  View()




# rs.bm <- rbenchmark::benchmark(velox = r_velox_sub$rasterize(spdf = ecoregions_sf, field = "ECO_ID", band = 3, background = -1),
#                                fasterize = fasterize::fasterize(sf = ecoregions_sf, raster = r_sub, background = NA, field = "ECO_ID", fun = "last"),
#                                replications = 5,
#                                columns = c("test", "replications", "elapsed", "relative"))
# rs.bm
# > rs.bm
# test replications elapsed relative
# 2 fasterize            5  20.321    2.633
# 1     velox            5   7.718    1.000



# 1. Make 30n arc-sec grid for tile 
r_intersects <- sf::st_bbox(r) %>% 
  sf::st_as_sfc() %>% 
  sf::st_make_grid(cellsize = c(134,134)) %>% 
  #dplyr::slice(1)

############################################

r <- raster::raster(hansen_tiles[13])
sf::st_bbox(r)
r_intersects <- sf::st_bbox(r) %>% 
  sf::st_as_sfc() %>% 
  sf::st_make_grid(cellsize = 133)
 
 # dplyr::slice(1)
  
  

# Make velox object 
r_velox <- velox::velox(r)
r_velox_sub <- velox::velox(r_sub)
# Add pixel area layer 

# Rasterize eco PM
r_velox$rasterize(...)
r_velox_sub$rasterize(spdf = ecoregions_sf, field = "ECO_ID", band = 3, background = -1)
# r_velox_sub$rasterize(spdf = protected_areas_sf, field = "WDPAID", band = 4, background = -1)
r_velox_sub$rasterbands[[1]]

# Rasterize protected 
r_velox$rasterize(...)
# Extract values 

names(r_velox_sub)
  
# 2. (skip) check if tile overlaps ecoregions
  
# 3. Rasterize ecoregions and protected areas 
r_eco <- fasterize::fasterize(sf = sf_obj, raster = r_template, background = background, field = field, fun = fun)
r_pro <- fasterize::fasterize(sf = sf_obj, raster = r_template, background = background, field = field, fun = fun)



# 2. estimate pixel area
ra <- raster::area(r, na.rm = FALSE, filename = paste0("./input/hansen/", names(r), "_area.tif"))

# 0. load hansen tile
r <- velox::velox(x = f)


