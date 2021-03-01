
# OSM Infrastructure Data

### osm_grab

- Download some pre-processed regional subsets from https://geofabrik.de
- Use osmconvert (<https://wiki.openstreetmap.org/wiki/Osmconvert>) and osmfilter (<https://wiki.openstreetmap.org/wiki/Osmfilter>) to convert from PBF to OSM and filter the results to roads and waterways

**Storage required can be around 100GB.** To limit this files from intermediate steps (PBF and unfiltered OSM) are deleted asap.

### gpkg_convert

- Convert the extracted OSM files to GPKG using GDAL (<https://www.gdal.org/>)

### SCP

- Use SCP to copy the resulting GPKG files to some storage accesible to Geoserver

### osm_geoserver

- Utilise fineprintutils to insert the files into Geoserver and create appropriate layers

