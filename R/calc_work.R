#' Calculate work from slope raster and body size
#'
#' @param slope is the slope raster
#' @param m is the body mass (kg) of the species
#' @param J_to_kcal is the Joule to kilocalory conversion contant
#' @param work_in_kcal if the output should be expressed in kcal
#' @return a raster of the energetic costs of locomotion
calc_work <- function(
  slope = NULL,
  m = NULL,
  J_to_kcal = 4184, #Joules to kilocalories
  work_in_kcal = FALSE
) {
  if (is.null(slope) | is.null(m)) {
    stop("Missing mandatory input.")
  }
  # split this in parts to be comparable with the paper
  E_cot <- 8 * m ^ (-0.34) + 100 * (1 + sin((2 * slope - 74) * pi / 180)) * m^(-0.12)
  if (class(slope) == "RasterLayer") { #raster
    dx <- dx <- raster::res(slope)[1]
  } else {
    stop("Resolution of slope raster not found.")
  }
  # h <- tan(deg_to_rad * slope) * res(slope)[1]
  # W <- m * g * h * eff #work (Joule)
  work <- m * E_cot * dx #work (J)
  if (work_in_kcal) {
    work <- work / J_to_kcal
  }
  return(work)
}
