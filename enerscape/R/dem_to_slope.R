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
  if (class(dem) == "RasterLayer") { #raster
    dem_file <- dem@file@name
  } else if (class(dem) == "SpatRaster") { #terra
    dem_file <- dem@ptr$filenames
  } else {
    if(class(dem) != "character") {
      stop("Provide an appropariate input - see ?dem_to_slope")
    }
  }
  if (is.null(dem_file) | is.na(dem_file) | dem_file == "") {
    #handling invalid file path
    stop("No file address in the raster file. Check:
         \t1. dem@file@name, if using raster
         \t2. dem@ptr$filenames, if using terra")
  }
  # this is a wrapper to 'gdal slope input output'
  if (output_to_disk) {
    if (is.null(output_file)) {
      stop("Specify a destination directory for the output.")
    }
    slope <- gdalUtils::gdaldem("slope",
                                dem_file,
                                file.path(output_file, "slope.tif"),
                                output_Raster = TRUE,
                                verbose = FALSE)
  } else {
    slope <- gdalUtils::gdaldem("slope",
                                dem_file,
                                paste0(base::tempfile(), "-slope.tif"),
                                output_Raster = TRUE,
                                verbose = FALSE)
  }
  if (class(dem) == "SpatRaster") {
    slope <- terra::rast(slope[[1]])
  } else {
    slope <- slope[[1]]
  }
  return(slope)
}
