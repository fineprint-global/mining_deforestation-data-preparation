resample_gdal <- function(src_file, dst_file, land_mask, r = "mode", 
                              co = list("compress=LZW", "TILED=YES"), 
                              verbose = TRUE, overwrite = TRUE, ...) {

  f_tmp <- raster::rasterTmpFile()
  
  land_mask <- raster::stack(land_mask)
  te_exten = as.vector(raster::extent(land_mask))[c(1,3,2,4)] 
  te_proj_srs = raster::projection(land_mask) 
  ts_col_row = c(ncol(land_mask), nrow(land_mask))
  
  # resample 
  system.time(
    gdalUtils::gdalwarp(srcfile = src_file, 
                        dstfile = f_tmp, 
                        r = r, 
                        co = co,
                        te = te_exten, 
                        te_srs = te_proj_srs, 
                        ts = ts_col_row, 
                        verbose = verbose, 
                        overwrite = TRUE, ...))
  
  # land mask 
  dst_file <- parllel_land_mask(x = raster::stack(list(f_tmp, land_mask)),
                                dst_file = dst_file, co = co, overwrite = overwrite, verbose = verbose)

    
  file.remove(f_tmp)
  return(dst_file)
  
}

