
# Download the data of Weiss et al. (2018) from
# https://map.ox.ac.uk/research-project/accessibility_to_cities/

download_file("https://map.ox.ac.uk/wp-content/uploads/accessibility/accessibility_to_cities_2015_v1.0.zip",
              "accessibility_to_cities_2015_v1-0.zip")
unzip("accessibility_to_cities_2015_v1-0.zip", files = "accessibility_to_cities_2015_v1.0.tif")
file.rename("accessibility_to_cities_2015_v1.0.tif", "accessibility_to_cities_2015_v1-0.tif")
file.remove("accessibility_to_cities_2015_v1-0.zip")

# Post it to Geoserver

library(fineprintutils)

gs_upload("accessibility_to_cities_2015_v1-0.tif", "weiss-etal", 
          "accessibility_to_cities", "2015", "v1-0")

xml_covstore <- gs_xml_covstore(
  name = "accessibility_to_cities_2015", 
  description = "Accessibility To Cities, 2015", 
  type = "GeoTIFF", 
  workspace = "weiss-2018", 
  path_gs = "file:data/rstudio/weiss-etal/accessibility_to_cities/2015/v1-0/accessibility_to_cities_2015_v1-0.tif")

xml_cov <- gs_xml_cov(
  name = "accessibility_to_cities_2015", 
  title = "Accessibility To Cities, 2015",
  abstract = "The economic and man-made resources that sustain human wellbeing are not distributed evenly across the world, but are instead heavily concentrated in cities. Poor access to opportunities and services offered by urban centres (a function of distance, transport infrastructure, and the spatial distribution of cities) is a major barrier to improved livelihoods and overall development. Advancing accessibility worldwide underpins the equity agenda of 'leaving no one behind' established by the Sustainable Development Goals of the United Nations. This has renewed international efforts to accurately measure accessibility and generate a metric that can inform the design and implementation of development policies. The only previous attempt to reliably map accessibility worldwide, which was published nearly a decade ago, predated the baseline for the Sustainable Development Goals and excluded the recent expansion in infrastructure networks, particularly in lower-resource settings. In parallel, new data sources provided by Open Street Map and Google now capture transportation networks with unprecedented detail and precision. Here we develop and validate a map that quantifies travel time to cities for 2015 at a spatial resolution of approximately one by one kilometre by integrating ten global-scale surfaces that characterize factors affecting human movement rates and 13,840 high-density urban centres within an established geospatial-modelling framework. Our results highlight disparities in accessibility relative to wealth as 50.9% of individuals living in low-income settings (concentrated in sub-Saharan Africa) reside within an hour of a city compared to 90.7% of individuals in high-income settings. By further triangulating this map against socioeconomic datasets, we demonstrate how access to urban centres stratifies the economic, educational, and health status of humanity.",
  keywords = c("weiss", "2015", "accessiblity", "travel", "time", "tif", "city", "cities"),
  type = "GeoTIFF",
  srs = "EPSG:4326",
  datetime = as.character(Sys.Date()),
  version = "v1-0",
  source_url = "https://www.nature.com/articles/nature25181/",
  dl_date = "2019-05-24",
  path = "/mnt/nfs_fineprint/data/geoserver/rstudio/weiss-etal/accessibility_to_cities/2015/v1-0/accessibility_to_cities_2015_v1-0.tif",
  licence = "CC-BY 4.0")

gs_post_xml(xml_covstore, "http://fineprint.wu.ac.at:8080/geoserver/rest/workspaces/weiss-2018/coveragestores")
gs_post_xml(xml_cov, "http://fineprint.wu.ac.at:8080/geoserver/rest/workspaces/weiss-2018/coveragestores/accessibility_to_cities_2015/coverages")
