
# Download the data of Hengl et al. (2017) from
# https://files.isric.org/soilgrids/data/recent/

url <- "https://files.isric.org/soilgrids/data/recent/TAXNWRB_250m_ll.tif"
filename <- "soil_classification_250m_TAXNWRB.tif"
download.file(url, filename)

# check the data
library(raster)
soilgrids <- raster::raster("soil_classification_250m_TAXNWRB.tif")
image(soilgrids)

# Post it to Geoserver

library(fineprintutils)

gs_upload("soil_classification_250m_TAXNWRB.tif", "soilgrids-2019", 
          "soil_classification_250m_TAXNWRB", "2017", "dl_2019-05")

xml_covstore <- gs_xml_covstore(
  name = "soil_classification_250m_TAXNWRB_2017", 
  description = "Most probable soil classification (TAXNWRB definition), 2017", 
  type = "GeoTIFF", 
  workspace = "soilgrids-2019", 
  path_gs = "file:data/rstudio/soilgrids-2019/soil_classification_250m_taxnwrb/2017/dl_2019-05/soil_classification_250m_TAXNWRB.tif")

xml_cov <- gs_xml_cov(
  name = "soil_classification_250m_TAXNWRB_2017", 
  title = "Soil classification (TAXNWRB definition), 2017",
  abstract = "This repository contains a collection of updatable soil property and class maps of the world at a relatively coarse resolution (1 km, 250 m etc) produced using automated soil mapping based on Machine Learning. SoilGrids is a global soil data product generated at ISRIC - World Soil Information as a result of international collaboration (see list of contributing agencies) and as a proposed contribution to the Global Soil Partnership (http://www.fao.org/globalsoilpartnership/en/) initiative.
  
  Important notice: the SoilGrids layers are of limited thematic (wide confidence limits) and spatial accuracy and still contain artefacts and missing pixels. Help us improve these maps by submitting a validation report or by contributing georeferenced soil profiles (points) and covariate data (rasters with global coverage at resolutions of 250 m or better). As soon as we receive a sufficient number of validation reports and/or new ground observations, we can re-run the predictions to produce an updated version of SoilGrids.
  
  This version of SoilGrids is licenced to you by ISRIC - World Soil Information strictly in accordance with the terms and conditions of the Open Data Commons Open Database License (ODbL) (http://opendatacommons.org/licenses/odbl/). By using the ISRIC website and web services the user accepts the ISRIC data policy (http://www.isric.org/data/data-policy) in full.
  
  Last update: 2017-August-10.
  Next scheduled update: March 2018.
  URL: www.soilgrids.org ",
  keywords = c("hengl", "2017", "soilgrids", "soil", "TAXNWRB", "tif"),
  type = "GeoTIFF",
  srs = "EPSG:4326",
  datetime = as.character(Sys.Date()),
  version = "dl_2019-05",
  source_url = "https://files.isric.org/soilgrids/",
  dl_date = "2019-05-27",
  path = "/mnt/nfs_fineprint/data/geoserver/rstudio/soilgrids-2019/soil_classification_250m_taxnwrb/2017/dl_2019-05/soil_classification_250m_TAXNWRB.tif",
  licence = "ODbL")

gs_post_xml(xml_covstore, "http://fineprint.wu.ac.at:8080/geoserver/rest/workspaces/soilgrids-2019/coveragestores")
gs_post_xml(xml_cov, "http://fineprint.wu.ac.at:8080/geoserver/rest/workspaces/soilgrids-2019/coveragestores/soil_classification_250m_TAXNWRB_2017/coverages")


### OLD STUFF


# data from https://soilgrids.org 
# API and R: https://medium.com/@traffordDataLab/querying-apis-in-r-39029b73d5f1
# soil grids API: https://rest.soilgrids.org/query.html 

# some initial trys ------------------------------------------------------

# ### access at point locations
# lon <- 5.39
# lat <- 51.57
# attr <- "TAXNWRB"
# path <- paste0("https://rest.soilgrids.org/query?lon=", lon, "&lat=", lat, "&attributes=", attr)
# 
# request <- httr::GET(url = path)
# request$status_code
# response <- httr::content(request, as = "text", encoding = "UTF-8")
# df <- jsonlite::fromJSON(response, flatten = TRUE) %>%
#   data.frame() %>% dplyr::select(type, geometry.type, geometry.coordinates, crs.type, crs.name, timestamp, raw,
#                                  properties.TAXNWRBMajor, properties.publication_date)
# df <- df %>%
#   dplyr::mutate(lon = df$geometry.coordinates[1], lat = df$geometry.coordinates[2]) %>%
#   dplyr::slice(1) %>%
#   dplyr::select(-geometry.coordinates)


# download data -----------------------------------------------------------

### project homepage https://www.isric.org/explore/soilgrids 
### tutorial https://github.com/ISRICWorldSoil/GSIF_tutorials/blob/master/SoilGrids250m/tutorial_SoilGrids250m.R

# 
# library(RCurl)
# sg.ftp <- "ftp://ftp.soilgrids.org/data/recent/"
# filenames = RCurl::getURL(sg.ftp, ftp.use.epsv = FALSE, dirlistonly = TRUE)
# filenames = strsplit(filenames, "\r*\n")[[1]]
# "TAXNWRB_250m.tif" %in% filenames
# 
# ## download to a local directory:
# ORC.name <- "TAXNWRB_250m.tif"
# try(download.file(paste(sg.ftp, ORC.name, sep=""), paste0("input/", ORC.name)))
# ## 500MB Geotiff!!
# 
# 
# url <- "https://files.isric.org/soilgrids/data/recent/TAXNWRB_250m_ll.tif"
# download.file(url, paste0("input/", "test"))

### alternatively download manually from https://files.isric.org/soilgrids/data/recent/ 



