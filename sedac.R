
# Download SEDAC data manually from
# https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-rev11/data-download


# check the data
library(raster)
popdens <- raster::raster("gpw_v4_population_density_rev11_2000_30_sec.tif")
image(popdens)


# Post it to Geoserver

library(fineprintutils)

for (yr in c(2000, 2005, 2010, 2015, 2020)){
  gs_upload(paste0("gpw_v4_population_density_rev11_", yr, "_30_sec.tif"),
            "sedac-2019", "population_density", yr, "v4-11-05")
}

for (yr in c(2000, 2005, 2010, 2015, 2020)){
  xml_covstore <- gs_xml_covstore(
    name = paste0("population_density_", yr), 
    description = paste0("Population density ", yr),
    type = "GeoTIFF", 
    workspace = "sedac-2019", 
    path_gs = paste0("file:data/rstudio/sedac-2019/population_density/", yr, "/v4-11-05/gpw_v4_population_density_rev11_", yr, "_30_sec.tif"))
  
  xml_cov <- gs_xml_cov(
    name = paste0("population_density_", yr), 
    title = paste0("Population density ", yr),
    abstract = "The Gridded Population of the World, Version 4 (GPWv4): Population Density, Revision 11 consists of estimates of human population density (number of persons per square kilometer) based on counts consistent with national censuses and population registers, for the years 2000, 2005, 2010, 2015, and 2020. A proportional allocation gridding algorithm, utilizing approximately 13.5 million national and sub-national administrative units, was used to assign population counts to 30 arc-second grid cells. The population density rasters were created by dividing the population count raster for a given target year by the land area raster. The data files were produced as global rasters at 30 arc-second (ca 1 km at the equator) resolution. To enable faster global processing, and in support of research communities, the 30 arc-second count data were aggregated to 2.5 arc-minute, 15 arc-minute, 30 arc-minute and 1 degree resolutions to produce density rasters at these resolutions.",
    keywords = c("sedac", "population", "population density", "Gridded Population of the World", "GWP", "tif"),
    type = "GeoTIFF",
    srs = "EPSG:4326",
    datetime = as.character(Sys.Date()),
    version = "v4-11-05",
    source_url = "https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-rev11",
    dl_date = "2019-05-27",
    path = paste0("/mnt/nfs_fineprint/data/geoserver/rstudio/sedac-2019/population_density/", yr, "/v4-11-05/gpw_v4_population_density_rev11_", yr, "_30_sec.tif"),
    licence = "CC BY 4-0")
  
  gs_post_xml(xml_covstore, "http://fineprint.wu.ac.at:8080/geoserver/rest/workspaces/sedac-2019/coveragestores")
  gs_post_xml(xml_cov, paste0("http://fineprint.wu.ac.at:8080/geoserver/rest/workspaces/sedac-2019/coveragestores/population_density_", yr, "/coverages"))
  
  cat(yr, "done")
}
