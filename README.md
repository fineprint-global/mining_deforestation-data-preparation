# Mining & Deforestation: data preparation 

The scripts in this repository produces the data for the statistical analysis <https://github.com/fineprint-global/mining_deforestation-stat> in *Mining is a major driver of deforestation across tropical forests* by Giljum et al. (Under review). 

- `download_osm/ ` download and process data from  [OpenStreetMap](OpenStreetMaphttps://www.openstreetmap.org)
- `download_urban/` download data on accessibility (Weiss et al. (2018) <https://map.ox.ac.uk/research-project/accessibility_to_cities/>) and ESA/CCI Land Cover Map for 2000 over their FTP, see <http://maps.elie.ucl.ac.be/CCI/viewer/download.php>
- `download_soilgrids/` Download soil grids <https://files.isric.org/soilgrids/latest/data/wrb/>
- `download_sedac/` Download population density from SEDAC <https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-rev11/data-download>
- `run_jobs` send jobs to cluster to process global 30 arcsec grid
- `run_pixel_area` send jobs to cluster to calculate pixel area 
- `00_build_global_forest_cover_vrt.R` Create global VRT for rest data 
- `02_calculate_distance_osm.R` compute distance to transport infrastructure 
- `03_additional_2000_layers.R` compute distance to agriculture
- `05_calculate_distance_to_the_sea.R` compute distance to coastline
- `06_multiscale_mining.R` compute mining area at multiple scales 
- `07_build_fineprint_30sec_grid.R` compute global 30 arcsec grid -- produces tiff and geojson files
- `08_bind_timeseries.R` merge forest loss time series 
- `R/` Supporting functions 
