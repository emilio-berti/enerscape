#' @param slope is the slope raster
#' @param m is the body mass (kg) of the species
#' @return a raster of the energetic costs of locomotion
calc_work <- function(
  slope = NULL,
  m = NULL,
  output_to_disk = FALSE,
  output_file = NULL,
  g = 9.80665, #gravitational pull
  deg_to_rad = 0.01745329, #degrees to radians conversion
  J_to_Kcal = 4184, #Joules to Kilocalories
  work_in_kcal = FALSE
  ) {
  if (is.null(slope) | is.null(m)) {
    stop("Missing mandatory input.")
  }
  # split this in parts to be comparable with the paper
  E_cot <- 8 * m^(-0.34) + 100 * (1 + sin(deg_to_rad * 2 * slope - deg_to_rad * 74)) * m^(-0.12) #joule / (kg * m)
  dx <- raster::res(slope)[1]
  # h <- tan(deg_to_rad * slope) * res(slope)[1]
  # W <- m * g * h * eff #work (Joule)
  W <- m * E_cot * dx #work (J)
  if (work_in_kcal) {
    W <- W / J_to_Kcal
  }
  if (output_to_disk) {
    if (is.null(output_file)) {
      stop("Specify a destination directory for the output.")
    }
    writeRaster(W, file.path(output_file, "work.tif"), overwrite = TRUE)
  }
  return(W)
}
