data("sirente")#' Compute Energy Landscapes
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
#' @param direction character specifying if costs are to be calcualted for
#'  moving into the focal cell (`in`) or from it (`out`).
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
#' data("sirente")
#' dem <- rast(sirente)
#' en <- enerscape(dem, 10, unit = "kcal", neigh = 8)
#' plot(en, col = hcl.colors(100, "Inferno"))
#' contour(dem, add = TRUE, nlevels = 5, col = hcl.colors(7, "Terrain"))
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
    neigh = 8,
    direction = "in"
) {
  if (is.null(dem) | is.null(m)) {
    stop("Missing mandatory input - see ?enerscape for details.")
  }
  if (!is(dem, "SpatRaster")) {
    stop("'dem' must be a SpatRaster (terra).")
  }
  if (!unit %in% c("joule", "kcal")) {
    stop("'unit' must be one of 'joule' or 'kcal'.")
  }
  if (neigh != 4 && neigh != 8) {
    stop("'neigh' should be either 4 or 8.")
  }
  if (direction != "in" && direction != "out") {
    stop("'direction' must be either 'in' or 'out'.")
  }
  # check units of DEM
  work_in_kcal <- ifelse(unit == "kcal", TRUE, FALSE)
  stopifnot( abs(res(dem)[1] - res(dem)[2]) <= 1e-2 )  # tolerance = 1 cm 
  en_res <- res(dem)[1]
  message("DEM is assumed to have planar CRS in meters.")
  out <- ifelse(direction == "in", 0L, 1L)
  if (out) {
    message("Costs are calculated for moving from the cell.")
  }
  x <- matrix(dem, nrow = nrow(dem), ncol = ncol(dem), byrow = TRUE)
  en <- energyscape(
    x,
    n = neigh,
    mass = m,
    kcal = work_in_kcal,
    res = en_res,
    out = out
  )
  ans <- rast(en)
  names(ans) <- "EnergyScape"
  crs(ans) <- crs(dem)
  ext(ans) <- ext(dem)
  ans[ans == 0] <- NA
  return (ans)
}
