#' Compute the energy landscape
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
#' @param method method to use to compute the energy costs. 'ARC' refers to the
#'   model from Pontzer (2016) and 'cycling' to the model for cyclist from di
#'   Prampero et al. (1979).
#' @param v speed of cyclist (km / h), only for \code{method = 'cycling'}.
#' @return A list with elements a rasterStack of the digital elevation model,
#'   slope, energy landscape, and conductance and the conductance as a transitionLayer for
#'   path analysis.
#' @details From the digital elevation model, transition slopes, energy costs
#'   and conductances (1 / work) are computed based on the model described in
#'   Pontzer (2016).
#' @examples
#' library(raster)
#' data("volcano")
#' dem <- raster(volcano)
#' en <- enerscape(dem, 10, unit = "kcal", neigh = 16)
#' @export
#' @references Etten, J. van. (2017). R Package gdistance: Distances and Routes
#'   on Geographical Grids. Journal of Statistical Software, 76(1), 1–21.
#'   \doi{https://doi.org/10.18637/jss.v076.i13}.
#'
#'   Pontzer, H. (2016). A unified theory for the energy cost of legged
#'   locomotion. Biology Letters, 12(2), 20150935. \doi{
#'   https://doi.org/10.1098/rsbl.2015.0935}.
#'
#'   di Prampero, P. E., Cortili, G., Mognoni, P., & Saibene, F. (1979).
#'   Equation of motion of a cyclist. Journal of Applied Physiology, 47(1),
#'   201–206. \doi{https://doi.org/10.1152/jappl.1979.47.1.201}
enerscape <- function(
  dem,
  m,
  unit = "joule",
  neigh = 16,
  method = "ARC",
  v = NULL
) {
  if (is.null(dem) | is.null(m)) {
    stop("Missing mandatory input - see ?enerscape::enerscape for details")
  }
  if (class(dem) != "RasterLayer") {
    stop("Digital elevation model must be a RaterLayer")
  }
  if (!unit %in% c("joule", "kcal")) {
    stop("unit must be one of 'joule' or 'kcal'")
  }
  work_in_kcal <- ifelse(unit == "kcal", TRUE, FALSE)
  # transition layers cannot accept optional arguments. The resolution is saved
  # as variable in this environment and called with 'get()' in
  # enerscape_internals.R
  en_res <- raster::res(dem)[1]
  message(" - Raster cells are assumed to have same horizontal and vertical",
          " resolution and with planar coordinate reference system (e.g. UTM)")
  message("  | Calculating slope")
  height <- suppressWarnings( #this warning is printed as message before return
    gdistance::transition(dem,
                          function(x) {
                            x[2] - x[1]
                          },
                          directions = neigh,
                          symm = FALSE)
  )
  slope <- gdistance::geoCorrection(height, scl = FALSE)
  slope <- atan(slope) * 180 / pi #convert slope ratio to degrees
  adj <- raster::adjacent(dem,
                          1:raster::ncell(dem),
                          pairs = TRUE,
                          directions = neigh)
  message("  | Calculating work")
  work <- slope
  if (method == "ARC") {
    work[adj] <- .calc_arc(slope[adj], m, work_in_kcal)
  } else if (method == "cycling") {
    if (is.null(v)) {
      stop("Argument v must be specified")
    } else {
      v <- v / 3.6
    }
    work[adj] <- .calc_cycling(height[adj], slope[adj], m, v, work_in_kcal)
    zeros <- Matrix::which(work@transitionMatrix < 0, arr.ind = TRUE)
    rw <- zeros[, 1]
    cl <- zeros[, 2]
    work@transitionMatrix[rw, cl] <- 0
  } else {
    stop("Argument method must be 'ARC' or 'cycling'")
  }
  message("  | Calculating conductance (1 / work)")
  cond <- slope
  if (method == "ARC") {
    cond[adj] <- .calc_arc_cond(slope[adj], m, work_in_kcal)
  } else if (method == "cycling") {
    cond[adj] <- .calc_cycling_cond(height[adj], slope[adj], m, v, work_in_kcal)
    zeros <- Matrix::which(cond@transitionMatrix < 0, arr.ind = TRUE)
    rw <- zeros[, 1]
    cl <- zeros[, 2]
    val <- max(cond@transitionMatrix) * 10
  } else {
    stop("Argument method must be 'ARC' or 'cycling'")
  }
  s <- gdistance::raster(slope, "colSums") / neigh
  w <- gdistance::raster(work, "colSums") / neigh
  if (method == "cycling") {
    w[w < 0] <- 0
  }
  con <- gdistance::raster(cond, "colSums") / neigh
  ans <- raster::stack(dem, s, w, con)
  names(ans) <- c("DEM", "Slope", "EnergyScape", "Conductance")
  ans <- list(neighbors = neigh,
              mass = m,
              rasters = ans,
              cond_tr = cond)
  class(ans) <- "enerscape"
  message(" - Do not use slope with negative values for distance calculations")
  return(ans)
}
