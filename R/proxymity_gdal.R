proxymity_gdal <- function(src_file, dst_file, land_mask, field = NULL, fun = "last", 
                           background = NA, by = NULL, co = list("compress=LZW", "TILED=YES"), 
                           verbose = TRUE, overwrite = TRUE, ...) {

  te_exten = as.vector(raster::extent(land_mask))[c(1,3,2,4)] 
  te_proj_srs = raster::projection(land_mask) 
  ts_col_row = c(ncol(land_mask), nrow(land_mask))
  
  # rasterize 
  system.time(
    f_tmp1 <- write_fasterize(sf = src_file, raster = raster::raster(land_mask), 
                            field = field, fun = fun, background = background, by = by, overwrite = TRUE))
  
  gc()
  
  # mask marine areas 
  f_tmp2 <- raster::rasterTmpFile()
  system.time(raster::clusterR(x = raster::stack(list(f_tmp1, land_mask)), 
                               fun = raster::overlay, args = list(fun = function(x, m) x * m), 
                               filename = f_tmp2, options = unlist(co), overwrite = TRUE, verbose = TRUE))
  
  # calculate distance map
  f_tmp3 <- raster::rasterTmpFile()
  system.time(
    system(paste0("gdal_proximity.py ", f_tmp2," ", f_tmp3,
                  " -values 1 -ot Float32 -co compress=LZW -co TILED=YES -distunits GEO -nodata NA -maxdist 1000000")))

  # mask distance to protected area
  system.time(
    parllel_land_mask(x = raster::stack(list(f_tmp3, land_mask)),
                      dst_file = dst_file, co = co, overwrite = overwrite, verbose = verbose))

  file.remove(f_tmp1)
  file.remove(f_tmp2)
  file.remove(f_tmp3)

  return(dst_file)

}


write_fasterize <- function(sf, raster, filename = NULL, field = NULL, fun = "last", background = NA, by = NULL, overwrite = TRUE, ...){
  
  if(is.null(filename)){
    filename <- raster::rasterTmpFile()
  }
  
  sf::st_read(sf) %>% 
    fasterize::fasterize(raster = raster, field = field, fun = fun, background = background, by = by) %>% 
    raster::writeRaster(filename = filename, overwrite = overwrite, ...)
  
  return(filename)
  
}

