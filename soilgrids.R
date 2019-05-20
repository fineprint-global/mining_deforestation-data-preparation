
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


library(RCurl)
sg.ftp <- "ftp://ftp.soilgrids.org/data/recent/"
filenames = RCurl::getURL(sg.ftp, ftp.use.epsv = FALSE, dirlistonly = TRUE)
filenames = strsplit(filenames, "\r*\n")[[1]]
"TAXNWRB_250m.tif" %in% filenames

## download to a local directory:
ORC.name <- "TAXNWRB_250m.tif"
try(download.file(paste(sg.ftp, ORC.name, sep=""), paste0("input/", ORC.name)))
## 500MB Geotiff!!


### alternatively download manually from https://files.isric.org/soilgrids/data/recent/ 


# compile data ------------------------------------------------------------

library(raster)
soilgrids <- raster::raster("./input/TAXNWRB_250m_ll.tif")


# insert to CKAN -----------------------------------------------------------

library(fineprintutils)
?ck_post_dataset





