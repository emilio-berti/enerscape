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
#' @return A list with elements a rasterStack of the digital elevation model,
#'   slope, work, and conductance and the conductance as a transitionLayer for
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
#' on Geographical Grids. Journal of Statistical Software, 76(1), 1–21.
#' \url{https://doi.org/10.18637/jss.v076.i13}.
#'
#' Pontzer, H. (2016). A unified theory for the energy cost of legged
#' locomotion. Biology Letters, 12(2), 20150935. \url{
#' https://doi.org/10.1098/rsbl.2015.0935}.
enerscape <- function(
  dem,
  m,
  unit = "joule",
  neigh = 16
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
  # as global variable and deleted before return. Deletion takes place in the
  # parent environment of the function.
  if ("en_res" %in% ls()) {
    stop("You have a variable called `en_res`\n
         Please rename it as something else")
  }
  assign("en_res", raster::res(dem)[1], envir = .GlobalEnv)
  message(" - Raster cells are assumed to have same horizontal and vertical",
          " resolution and with planar coordinate reference system (e.g. UTM)")
  oldw <- getOption("warn")
  options("warn" = -1)
  message("  | Calculating slope")
  # slope <- gdistance::transition(dem, .calc_slope,
  #                                directions = neigh,
  #                                symm = FALSE)
  height <- gdistance::transition(dem,
                                  function(x) {
                                    x[2] - x[1]
                                  },
                                  directions = neigh,
                                  symm = FALSE)
  slope <- gdistance::geoCorrection(height, scl = FALSE)
  slope <- atan(slope) * 180 / pi #convert slope ratio to degrees
  options("warn" = oldw)
  adj <- raster::adjacent(dem,
                          1:raster::ncell(dem),
                          pairs = TRUE,
                          directions = neigh)
  message("  | Calculating work")
  work <- slope
  work[adj] <- .calc_work(slope[adj], m, work_in_kcal = work_in_kcal)
  message("  | Calculating conductance (1 / work)")
  cond <- slope
  cond[adj] <- .calc_cond(slope[adj], m, work_in_kcal = work_in_kcal)
  s <- gdistance::raster(slope, "colSums") / neigh
  w <- gdistance::raster(work, "colSums") / neigh
  con <- gdistance::raster(cond, "colSums") / neigh
  ans <- raster::stack(dem, s, w, con)
  names(ans) <- c("DEM", "Slope", "Work", "Conductance")
  ans <- list(neighbors = neigh,
              mass = m,
              rasters = ans,
              cond_tr = cond)
  class(ans) <- "enerscape"
  rm(en_res, envir = .GlobalEnv) #remove global variable
  message(" - Do not use slope with negative values for distance calculations")
  return(ans)
}
