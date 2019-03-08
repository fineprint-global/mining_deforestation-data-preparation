# 30-arc-second cells are of sufficient resolution and result in errors of less than 0.5 percent in categorical areas. 
# Source: https://www.researchgate.net/publication/250016006_Projecting_Global_Datasets_to_Achieve_Equal_Areas
# Hansen maps have resolution of 1 arc-second per pixel, or approximately 30 meters per pixel at the equator.
# Proje strings: https://proj4.org/index.html
# Interrupted Goode Homolosine: http://proceedings.esri.com/library/userconf/proc98/proceed/TO850/PAP844/P844.HTM

library(fineprintutils)
library(raster)
library(tidyverse)
library(sf)
library(spData)
raster::rasterOptions(progress = 'text')

# Projection: Goode Homolosine
spData::world %>% 
  sf::st_transform(crs = sf::st_crs("+proj=goode")) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(geometry = geom)) + 
  ggthemes::theme_map()

# Projection: Interrupted Goode Homolosine
spData::world %>% 
  sf::st_transform(crs = sf::st_crs("+proj=igh"), partial = FALSE) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(geometry = geom)) + 
  ggthemes::theme_map()

# Projection: Mollweide
spData::world %>% 
  sf::st_transform(crs = sf::st_crs("+proj=moll")) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(geometry = geom)) + 
  ggthemes::theme_map()


# Test reprojection of Hansen data 
fineprintutils::download_file(url = "https://storage.googleapis.com/earthenginepartners-hansen/GFC-2017-v1.5/Hansen_GFC-2017-v1.5_lossyear_00N_060W.tif", 
              destfile = "./input/hansen/Hansen_GFC-2017-v1.5_lossyear_00N_060W.tif", mode = "wb")
fineprintutils::download_file(url = "https://storage.googleapis.com/earthenginepartners-hansen/GFC2015/Hansen_GFC2015_lossyear_70N_030E.tif", 
              destfile = "./input/hansen/Hansen_GFC-2017-v1.5_lossyear_70N_030E.tif", mode = "wb")


# Using 12 cores in the wucloud server it takes about an hour to reproject one deforestation tile. 
# We have 509 tiles to process, potentially we could reproject about 24/day, which will take about 20 days.
r_wgs84_llat <- raster::raster("./input/hansen/Hansen_GFC-2017-v1.5_lossyear_00N_060W.tif")
r_wgs84_hlat <- raster::raster("./input/hansen/Hansen_GFC-2017-v1.5_lossyear_70N_030E.tif")
raster::beginCluster(n = 12)
t0 <- proc.time()
r_igh_llat <- raster::projectRaster(r_wgs84_llat, res = c(30, 30), crs = "+proj=igh", method = "ngb", filename = "./input/hansen/Hansen_GFC-2017-v1.5_lossyear_00N_060W_igh.tif")
r_igh_hlat <- raster::projectRaster(r_wgs84_hlat, res = c(30, 30), crs = "+proj=igh", method = "ngb", filename = "./input/hansen/Hansen_GFC-2017-v1.5_lossyear_70N_030E_igh.tif")
t1 <- proc.time()
raster::endCluster()
time_reproject <- t1 - t0

# Calculate area non-projected
rasterOptions(maxmemory = 1e+04)
r_list <- list(r_wgs84_llat = raster::raster("./input/hansen/Hansen_GFC-2017-v1.5_lossyear_00N_060W.tif"),
               r_wgs84_hlat = raster::raster("./input/hansen/Hansen_GFC-2017-v1.5_lossyear_70N_030E.tif"))
t0 <- proc.time()
r_area_list <- parallel::mclapply(r_list, mc.cores = length(r_list), function(r){
  raster::area(r, na.rm = FALSE, filename = paste0("./input/hansen/", names(r), "_area.tif"))
})
area_list <- parallel::mclapply(seq_along(r_list), mc.cores = length(r_list), function(i){
  raster::zonal(x = r_area_list[[i]], z = r_list[[i]], fun = 'sum', na.rm = TRUE) %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(area = sum) # km2
})
names(area_list) <- names(r_list)
t1 <- proc.time()
time_raster_area <- t1 - t0
save(area_list, file = "./output/area_list.RData")

# Calculate area using frequency and resolution
rasterOptions(maxmemory = 1e+04)
r_list <- c(r_list,
            r_igh_llat = raster::raster("./input/hansen/Hansen_GFC-2017-v1.5_lossyear_00N_060W_igh.tif"),
            r_igh_hlat = raster::raster("./input/hansen/Hansen_GFC-2017-v1.5_lossyear_70N_030E_igh.tif"))
t0 <- proc.time()
freq_list <- parallel::mclapply(r_list, mc.cores = length(r_list), function(r){
  raster::freq(r) %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(area = count * 30 * 30 * 1e-6) # km2
})
t1 <- proc.time()
time_area_freq <- t1 - t0
save(freq_list, file = "./output/freq_list.RData")


# Approximate surface area
load("./output/freq_list.RData")
load("./output/area_list.RData")
area_list$r_wgs84_llat <- dplyr::transmute(area_list$r_wgs84_llat, Group = zone, Area = area, Region = "Low latitude", Method = "Approximate surface area")
area_list$r_wgs84_hlat <- dplyr::transmute(area_list$r_wgs84_hlat, Group = zone, Area = area, Region = "High latitude", Method = "Approximate surface area")
# Projected/simple area 
freq_list$r_wgs84_llat <- dplyr::transmute(freq_list$r_wgs84_llat, Group = value, Area = area, Region = "Low latitude", Method = "Non projected pixel area")
freq_list$r_wgs84_hlat <- dplyr::transmute(freq_list$r_wgs84_hlat, Group = value, Area = area, Region = "High latitude", Method = "Non projected pixel area")
freq_list$r_igh_llat <- dplyr::transmute(freq_list$r_igh_llat, Group = value, Area = area, Region = "Low latitude", Method = "Projected pixel area")
freq_list$r_igh_hlat <- dplyr::transmute(freq_list$r_igh_hlat, Group = value, Area = area, Region = "High latitude", Method = "Projected pixel area")

area_tbl <- c(area_list, freq_list) %>% 
  dplyr::bind_rows()

area_tbl %>% 
  dplyr::filter(Group != 0, !is.na(Group)) %>%
  dplyr::filter(!is.na(Group)) %>% 
  ggplot(aes(x = Group, y = Area, color = Method)) + 
  facet_grid(. ~ Region, scales = "free_y", space = "free_y") + 
  geom_line() +
  geom_point() + 
  ggplot2::theme(legend.position = "bottom", legend.justification = "center")
  
area_tbl %>% 
  dplyr::filter(!is.na(Group)) %>% 
  dplyr::group_by(Method, Region) %>% 
  dplyr::summarise(Area = sum(Area, na.rm = TRUE)) %>% 
  dplyr::mutate(perc_diff = Area / c(523051, 1232904))

# We should use Approximate surface area from raster::area or from Google Earth Engine 


# For speed in R consider using velox https://cran.r-project.org/web/packages/velox/index.html
# r <- velox("testras_so.tif")
# r$crop(poly)
# r$extract(poly)
