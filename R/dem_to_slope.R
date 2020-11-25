#' Calculate slope from digital elevation model
#'
#' @inherit enerscape
#' @return raster of slope in degrees
dem_to_slope <- function(
  dem,
  neigh
) {
  if (class(dem) == "RasterLayer") {
    if (raster::res(dem)[1] == raster::res(dem)[2]) {
      slope <- raster::terrain(dem,
                               "slope",
                               neighbors = 4,
                               unit = "degrees")
    } else {
      stop("Digital elevation model has different x-y resolution")
    }
  } else {
    stop("Provide a digital elevation model as a RasterLayer")
  }
  return(slope)
}
