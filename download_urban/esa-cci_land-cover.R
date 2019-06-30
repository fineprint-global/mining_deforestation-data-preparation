
# Download the ESA/CCI Land Cover Map for 2000 over their FTP, see
# http://maps.elie.ucl.ac.be/CCI/viewer/download.php
download_file("ftp://geo10.elie.ucl.ac.be/v207/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7.tif",
              "esacci_lc_2000_v2-0-7.tif")
writeLines(
  "Terms of use
  
  The present products are made available to the public by ESA and the consortium. You may use one or several CCI-LC products land cover map for educational and/or scientific purposes, without any fee on the condition that you credit the ESA Climate Change Initiative and in particular its Land Cover project as the source of the CCI-LC database:
  
  Copyright notice:
  © ESA Climate Change Initiative - Land Cover led by UCLouvain (2017)
  
  Should you write any scientific publication on the results of research activities that use one or several CCI-LC products as input, you shall acknowledge the ESA CCI Land Cover project in the text of the publication and provide the project with an electronic copy of the publication (contact@esa-landcover-cci.org).
  
  If you wish to use one or several CCI-LC products in advertising or in any commercial promotion, you shall acknowledge the ESA CCI Land Cover project and you must submit the layout to the project for approval beforehand (contact@esa-landcover-cci.org).",
  "esa_cci_lc_LICENSE.txt"
)

# Post it to Geoserver

library(fineprintutils)

gs_upload(c("esacci_lc_2000_v2-0-7.tif", "esa_cci_lc_LICENSE.txt"), 
          "esa-cci", "land-cover", "2000", "v2-0-7")

xml_covstore <- gs_xml_covstore(
  name = "esacci_land-cover_2000", 
  description = "ESA/CCI Land Cover, 2000", 
  type = "GeoTIFF", 
  workspace = "esacci-2019", 
  path_gs = "file:data/rstudio/esa-cci/land-cover/2000/v2-0-7/esacci_lc_2000_v2-0-7.tif")

xml_cov <- gs_xml_cov(
  name = "esacci_land-cover_2000", 
  title = "ESA/CCI Land Cover, 2000",
  abstract = "24 consistent global land cover maps at 300 m spatial resolution, on an annual basis from 1992 to 2015. The annual LC maps v2.0.7 for years 2000, 2005 and 2010 replace completely the 'v1.6.1 epoch-based' dataset as the annual LC maps v2.0.7 have been improved both in the representation of the areas stable over time and in the characterization of change. Each pixel value corresponds to the label of a land cover class defined based on the UN Land Cover Classification System (LCCS). LCCS classifiers support the further conversion into Plant Functional Types distribution required by the Earth System Models. The typology counts 22 classes. The 24 LC maps series is delivered along with 4 quality flags which document the products.
  
  Broad classification:
  
  - 0: No data
  - 10-20: Cropland
  - 30-40: Mosaic cropland / Natural vegetation
  - 50-90: Tree cover
  - 100-122: Shrublands
  - 130: Grasslands
  - 140-153: Sparse vegetation
  - 160-180: Flooded areas
  - 190: Urban areas
  - 200-202: Bare areas
  - 210: Water bodies
  - 220: Permanent ice and snow",
  keywords = c("esa", "cci", "2000", "land", "cover", "map", "tif", "esacci"),
  type = "GeoTIFF",
  srs = "EPSG:4326",
  datetime = as.character(Sys.Date()),
  version = "v2-0-7",
  source_url = "http://maps.elie.ucl.ac.be/CCI/",
  dl_date = "2019-05-24",
  path = "/mnt/nfs_fineprint/data/geoserver/rstudio/esa-cci/land-cover/2000/v2-0-7/esacci_lc_2000_v2-0-7.tif",
  licence = "Custom",
  licence_path = "/mnt/nfs_fineprint/data/geoserver/rstudio/esa-cci/land-cover/2000/v2-0-7/esa_cci_lc_LICENSE.txt")

gs_post_xml(xml_covstore, "http://fineprint.wu.ac.at:8080/geoserver/rest/workspaces/esacci-2019/coveragestores")
gs_post_xml(xml_cov, "http://fineprint.wu.ac.at:8080/geoserver/rest/workspaces/esacci-2019/coveragestores/esacci_land-cover_2000/coverages")
