#' #' Calculate work from slope raster and body size
#' #'
#' #' @inherit enerscape
#' #' @param slope is the slope raster.
#' #' @param j_to_kcal is the Joule to kilocalory conversion contant.
#' #' @param work_in_kcal if the output should be expressed in kcal.
#' #' @return a raster of the energetic costs of locomotion.
#' calc_work <- function(
#'   slope = NULL,
#'   m = NULL,
#'   j_to_kcal = 4184, #Joules to kilocalories
#'   work_in_kcal = FALSE
#' ) {
#'   if (is.null(slope) | is.null(m)) {
#'     stop("Missing mandatory input - see ?enerscape::calc_work for details")
#'   }
#'   # split this in parts to be comparable with the paper
#'   E_cot <- 8 * m ^ (-0.34) + 100 * (1 + sin((2 * slope - 74) * pi / 180)) * m^(-0.12)
#'   dx <- raster::res(slope)[1]
#'   work <- m * E_cot * dx
#'   if (work_in_kcal) {
#'     work <- work / j_to_kcal
#'   }
#'   return(work)
#' }
