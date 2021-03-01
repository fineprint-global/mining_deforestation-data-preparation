#!/bin/bash

SUBSETS=(africa asia australia-oceania central-america south-america)

for SUBSET in "${SUBSETS[@]}"; do

  echo "Converting ${SUBSET}."
  ogr2ogr -f GPKG ${SUBSET}.gpkg ${SUBSET}_filtered.osm

done
