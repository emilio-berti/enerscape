#' Calculate slope from digital elevation model
#'
#' @param dem raster file of the digital elevation model, either a raster
#'   or a full path location of the file
#' @return raster of slope in degrees
dem_to_slope <- function(
  dem
) {
  if (class(dem) == "RasterLayer") {
    slope <- raster::terrain(dem,
                             "slope",
                             neighbors = 4,
                             unit = "degrees")
  } else {
    if(class(dem) != "character") {
      stop("Provide an appropariate input - see ?dem_to_slope")
    }
  }
  return(slope)
}
