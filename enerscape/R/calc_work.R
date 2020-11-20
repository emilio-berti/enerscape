#' Calculate work from slope raster and body size
#'
#' @param slope is the slope raster
#' @param m is the body mass (kg) of the species
#' @param output_to_disk if to write output to disk
#' @param output_file is the output destiation, if @param output_to_disc == TRUE
#' @param g is the gravity force
#' @param deg_to_rad is the degree to radiant conversion constant
#' @param J_to_Kcal is the Joule to kilocalory conversion contant
#' @param work_in_kcal if the output should be expressed in kcal
#' @return a raster of the energetic costs of locomotion
calc_work <- function(
  slope = NULL,
  m = NULL,
  output_to_disk = FALSE,
  output_file = NULL,
  g = 9.80665, #gravitational pull
  J_to_Kcal = 4184, #Joules to kilocalories
  work_in_kcal = FALSE
) {
  if (is.null(slope) | is.null(m)) {
    stop("Missing mandatory input.")
  }
  # split this in parts to be comparable with the paper
  if (class(slope) == "SpatRaster") {
    requireNamespace("terra") #without loading terra, the raster calculation fails
  }
  E_cot <- 8 * m^(-0.34) + 100 * (1 + sin((2 * slope - 74) * pi / 180)) * m^(-0.12) #joule / (kg * m)
  if (class(slope) == "RasterLayer") { #raster
    dx <- dx <- raster::res(slope)[1]
  } else if (class(slope) == "SpatRaster") { #terra
    dx <- slope@ptr$res[1]
  } else {
    stop("Resolution of slope raster not found.")
  }
  # h <- tan(deg_to_rad * slope) * res(slope)[1]
  # W <- m * g * h * eff #work (Joule)
  work <- m * E_cot * dx #work (J)
  if (work_in_kcal) {
    work <- work / J_to_Kcal
  }
  if (output_to_disk) {
    if (is.null(output_file)) {
      stop("Specify a destination directory for the output.")
    }
    if (class(slope) == "RasterLayer") {
      raster::writeRaster(work, file.path(output_file, "work.tif"), overwrite = TRUE)
    } else if (class(slope) == "SpatRaster") {
      terra::writeRaster(work, file.path(output_file, "work.tif"), overwrite = TRUE)
    } else {
      stop("Error, not writing to file.")
    }
  }
  return(work)
}
