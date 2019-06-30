#!/bin/bash

# SUBSETS=(africa antarctica asia australia-oceania central-america europe north-america south-america)
SUBSETS=(africa asia australia-oceania central-america south-america)

for SUBSET in "${SUBSETS[@]}"; do

  CHECK=$(curl -o /dev/null --silent --head --write-out "%{http_code}" "https://download.geofabrik.de/${SUBSET}-latest.osm.pbf")

  if [ ${CHECK} == "200" ]; then

    echo "$(date +'%T'): Downloading ${SUBSET}.\n"
    curl -o ${SUBSET}.osm.pbf "https://download.geofabrik.de/${SUBSET}-latest.osm.pbf"

    echo "$(date +'%T'): Converting from PBF to OSM.\n"
    osmconvert ${SUBSET}.osm.pbf > ${SUBSET}.osm
    rm ${SUBSET}.osm.pbf

    echo "$(date +'%T'): Filtering ${SUBSET}.\n"
    osmfilter ${SUBSET}.osm --keep-ways= --keep="highway=motorway =trunk =primary =secondary waterway=river natural=coastline admin_level=2" > ${SUBSET}_filtered.osm
    rm ${SUBSET}.osm

    else

    echo "$(date +'%T'): ${SUBSET} not found. Skipping.\n"

  fi

done
