#' Calculate slope from digital elevation model
#'
#' @param dem raster file of the digital elevation model, either a raster
#'   or a full path location of the file
#' @param output_to_disk (optional) specifies if the slope raster should be also
#'   saved to disk
#' @param output_file (optional) specifies the location of the output file. This
#'   must be a full path, e.g. "/home/user/Documents"
#' @return raster of slope in degrees
#' @details If @param output_to_disc = FALSE, the raster output will be saved in
#'   the temporary R folder, which is deleted at reboot. If @output_to_disc =
#'   TRUE, then @output_file must be specified.

dem_to_slope <- function(
  dem,
  output_to_disk = FALSE,
  output_file = NULL
) {
  if (class(dem) == "RasterLayer") {
    slope <- raster::terrain(dem,
                             "slope",
                             neighbors = 4,
                             unit = "degrees")
  } else if (class(dem) == "SpatRaster") {
    slope <- terra::slope(dem,
                          neighbors = 4,
                          unit = "degrees")
  } else {
    if(class(dem) != "character") {
      stop("Provide an appropariate input - see ?dem_to_slope")
    }
  }
  if (output_to_disk) {
    if (is.null(output_file)) {
      stop("Specify a destination directory for the output.")
    }
    if (class(dem) == "RasterLayer") { #raster
      raster::writeRaster(slope,
                          file.path(output_file, "slope.tif"),
                          output_Raster = TRUE)
    } else {
      terra::writeRaster(slope,
                          file.path(output_file, "slope.tif"),
                          output_Raster = TRUE)
    }

  }
  return(slope)
}
