#' Compute Energy Landscapes
#'
#' This is the main function to compute energy landscapes from a digital
#' elevation model and body mass of animals based on the model from Pontzer
#' (2016). The core of the computations are done using the \emph{gdistance}
#' (Etten, 2017) package.
#' @param dem raster file of the digital elevation model, either a raster or a
#'   full path location of the file.
#' @param m species body mass (kg).
#' @param unit if joules ('joule') or kilocalories ('kcal').
#' @param neigh number of neighbor cells that are connected together.
#' @return A list with elements a rasterStack of the digital elevation model,
#'   slope, energy landscape, and conductance and the conductance as a transitionLayer for
#'   path analysis.
#' @details From the digital elevation model, transition slopes, energy costs
#'   and conductances (1 / work) are computed based on the model described in
#'   Pontzer (2016).
#' @examples
#' library(terra)
#' library(enerscape)
#'
#' data("volcano")
#' dem <- rast(volcano)
#' en <- enerscape(dem, 10, unit = "kcal", neigh = 16)
#' @export
#' @references
#'   Pontzer, H. (2016). A unified theory for the energy cost of legged
#'   locomotion. Biology Letters, 12(2), 20150935. \doi{
#'   https://doi.org/10.1098/rsbl.2015.0935}.
#'
enerscape <- function(
    dem,
    m,
    unit = "joule",
    neigh = 8
) {
  if (is.null(dem) | is.null(m)) {
    stop("Missing mandatory input - see ?enerscape::enerscape for details.")
  }
  if (!is(dem, "SpatRaster")) {
    stop("Digital elevation model must be a SpatRaster (terra).")
  }
  if (!unit %in% c("joule", "kcal")) {
    stop("unit must be one of 'joule' or 'kcal'.")
  }
  # check units of DEM
  work_in_kcal <- ifelse(unit == "kcal", TRUE, FALSE)
  stopifnot( abs(res(dem)[1] - res(dem)[2]) <= 1e-2 ) #tolerance = 1 cm 
  en_res <- res(dem)[1]
  message("DEM is assumed to have planar CRS in meters.")
  x <- matrix(dem, nrow = nrow(dem), ncol = ncol(dem), byrow = TRUE)
  en <- energyscape(x,
                    n = neigh,
                    mass = m,
                    kcal = work_in_kcal,
                    res = en_res)
  ans <- rast(en)
  names(ans) <- "EnergyScape"
  crs(ans) <- crs(dem)
  ext(ans) <- ext(dem)
  ans[ans == 0] <- NA
  return (ans)
}
