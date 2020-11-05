#' @param slope is the slope raster
#' @param m is the body mass (g) of the species
#' @return a raster of the energetic costs of locomotion
calc_work <- function(
  slope = NULL,
  m = NULL,
  output_to_disk = FALSE,
  output_file = NULL,
  g = 9.80665, #gravitational pull
  deg_to_rad = 0.01745329, #degrees to radians conversion
  beta = 0.15 #scaling coefficient for locomotion efficiency  - filler, to be determined
  ) {
  if (is.null(slope) | is.null(m)) {
    stop("Missing mandatory input.")
  }
  eff <- m ^ beta
  h <- sin(deg_to_rad * slope)
  W <- m * g * h * eff #work (Joule)
  if (output_to_disk) {
    if (is.null(output_file)) {
      stop("Specify a destination directory for the output.")
    }
    writeRaster(W, file.path(output_file, "work.tif"), overwrite = TRUE)
  }
  return(W)
}
