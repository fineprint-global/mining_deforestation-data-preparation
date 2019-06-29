parllel_land_mask <- function(x, dst_file, co = list("compress=LZW", "TILED=YES"), 
                              overwrite = TRUE, verbose = TRUE, ...) {
  
  system.time(raster::clusterR(x = x, fun = raster::overlay, args = list(fun = function(x, m) ifelse(is.na(x), 0 * m, x * m)), 
                               filename = dst_file, options = unlist(co), overwrite = overwrite, verbose = verbose, ...))
  
  return(dst_file)
  
}
