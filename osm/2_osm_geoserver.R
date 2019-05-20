 
library(fineprintutils)

regions <- c("australia-oceania", "asia", "central-america", "south-america", "africa")
regions_cap <- gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(regions), perl = TRUE)


# DataStore ---------------------------------------------------------------

for(i in seq_along(regions)) {
  gs_post_xml(
    gs_xml_covstore(
      name = paste0("infrastructure_", regions[i], "_osm"),
      description = "Filtered OpenStreetMap data on infrastructure (roads & waterways). The applied filter (using osmfilter) is: `--keep-ways= --keep='highway=motorway =trunk =primary =secondary waterway=river natural=coastline admin_level=2'`.",
      type = "GeoPackage",
      workspace = "openstreetmap-2019",
      path_gs = paste0("file:data/openstreetmap/infrastructure/2019/dl_2019-05/", regions[i], ".gpkg")
    ),
    url = "http://fineprint.wu.ac.at:8080/geoserver/rest/workspaces/openstreetmap-2019/datastores"
  )
}


# Layers ---------------------------------------------------------------

layertypes <- c("lines", "multilinestrings", "multipolygons", "other_relations", "points")
layertypes_cap <- gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(layertypes), perl = TRUE)

for(i in seq_along(regions)) {
  for(j in seq_along(layertypes)) {
    cat(regions[i], layertypes[i], "-", gs_post_xml(
      gs_xml_cov(
        name = paste0("infrastructure_", regions[i], "_", layertypes[j]),
        title = paste0("Infrastructure, ", regions_cap[i], ", OSM, ", layertypes_cap[j]),
        abstract = "Filtered OpenStreetMap data on infrastructure (roads & waterways). The applied filter (using osmfilter) is: `--keep-ways= --keep='highway=motorway =trunk =primary =secondary waterway=river natural=coastline admin_level=2'`.",
        keywords = c("openstreetmap", "infrastructure", regions[i], "highway", "road", "waterway"),
        type = "GeoPackage",
        srs = "EPSG:4326",
        datetime = as.character(Sys.Date()),
        version = "v2",
        source_url = "https://www.geofabrik.de/",
        dl_date = "2019-05-07",
        path = "/mnt/nfs_fineprint/data/geoserver/openstreetmap/infrastructure/2019/dl_2019-05/australia-oceania.gpkg",
        layertype = layertypes[j],
        datastore = paste0("infrastructure_", regions[i], "_osm"),
        licence = "CC BY-SA 2.0"
      ),
      url = paste0("http://fineprint.wu.ac.at:8080/geoserver/rest/workspaces/openstreetmap-2019/datastores/", 
                   "infrastructure_", regions[i], "_osm/featuretypes")
      ), "\n")
  }
}
