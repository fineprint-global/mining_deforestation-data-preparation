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
  
  # project open street map to Equidistant Cylindrical (Plate Carrée)
  f_tmp3 <- raster::rasterTmpFile()
  system.time(gdalUtils::gdalwarp(srcfile = f_tmp2, r = "near", 
                                  dstfile = f_tmp3, t_srs = "+proj=eqc", co = list("compress=LZW", "TILED=YES"), 
                                  verbose = TRUE, overwrite = TRUE))
    
  # calculate distance map
  f_tmp4 <- raster::rasterTmpFile()
  system.time(
    system(paste0("gdal_proximity.py ", f_tmp3," ", f_tmp4,
                  " -values 1 -ot Float32 -co compress=LZW -co TILED=YES -distunits GEO -nodata NA -maxdist 15000000")))

  # resample raster back to longlat 
  f_tmp5 <- raster::rasterTmpFile()
  system.time(gdalUtils::gdalwarp(srcfile = f_tmp4, r = "bilinear", 
                                  dstfile = f_tmp5, 
                                  te = te_exten, 
                                  ts = ts_col_row, 
                                  t_srs = "+proj=longlat", 
                                  co = list("compress=LZW", "TILED=YES"), 
                                  verbose = TRUE, overwrite = TRUE))
  
  # mask distance to protected area
  system.time(
    parllel_land_mask(x = raster::stack(list(f_tmp5, land_mask)),
                      dst_file = dst_file, co = co, overwrite = overwrite, verbose = verbose))

  return(dst_file)

}


write_fasterize <- function(sf, raster, filename = NULL, field = NULL, fun = "last", background = NA, by = NULL, overwrite = TRUE, ...){
  
  if(is.null(filename)){
    filename <- raster::rasterTmpFile()
  }
  
  if(is(sf, "sf")){
    fasterize::fasterize(sf = sf, raster = raster, field = field, fun = fun, background = background, by = by) %>% 
      raster::writeRaster(filename = filename, overwrite = overwrite, ...)    
  } else {
    sf::st_read(sf) %>% 
      fasterize::fasterize(raster = raster, field = field, fun = fun, background = background, by = by) %>% 
      raster::writeRaster(filename = filename, overwrite = overwrite, ...)
  }
  
  return(filename)
  
}

