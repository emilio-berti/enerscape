#' @param dem_file raster file of the digital elevation model
#' @param slope_output is the file destination where to save the slope raster
#' @return raster of slope in degrees

dem_to_slope <- function(dem_file, slope_output) {
  for (x in c("raster", "gdalUtils")) {
    if (!require(x, character.only = TRUE)) {
      stop("Package ", x, " not installed, aborting.")
    }
  }
  # this is a wrapper to gdal slope input output
  slope <- gdalUtils::gdaldem("slope",
                     dem_file,
                     slope_output,
                     output_Raster = TRUE,
                     verbose = TRUE)
  return(slope)
}
