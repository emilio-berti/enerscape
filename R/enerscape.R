#' Internal function for enerscape - calculate slope
#' @param x a digital elevation model raster layer
.calc_slope <- function(x) {
  diff <- x[2] - x[1]
  ipo <- sqrt(en_res ^ 2 + diff ^ 2)
  alpha <- asin(diff / ipo) * 180 / pi
  return(alpha)
}
#' Internal function for enerscape - calculate work
#' @inherit enerscape
#' @param x a slope transition layer
#' @param work_in_kcal if work should be expressed in kilocalories
#' @param j_to_kcal joules to kilocalories conversion constant
.calc_work <- function(
  dem,
  m,
  work_in_kcal = TRUE,
  j_to_kcal = 4184
) {
  if (!all(is.na(dem))) {
    work <- 8 * m ^ (-0.34) + 100 * (1 + sin((2 * dem - 74) / 180 * pi)) * m ^ (-0.12)
  } else {
    work <- 8 * m ^ (-0.34)
  }
  work <- m * work * en_res
  if (work_in_kcal) {
    work <- work / j_to_kcal
  }
  return(work)
}
#' Calculate the energy landscape
#'
#' @param dem raster file of the digital elevation model, either a raster or a
#'   full path location of the file.
#' @param m species body mass (kg).
#' @param unit if joules ('joule') or kilocalories ('kcal').
#' @param neigh number of neighbors used to calculate slope and work. See
#'   ?raster::terrain for details.
enerscape <- function(
  dem,
  m,
  unit = "joule",
  neigh = 4
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
  work_in_kcal <- ifelse(unit == "joule", FALSE, TRUE)
  # transition layers cannot accept optional arguments. The resolution is saved
  # as global variable and deleted before return. Deletion takes place in the
  # parent environment of the function.
  if ("en_res" %in% ls()) {
    stop("You have a variable called `en_res`\n
         Please rename it as something else")
  }
  en_res <<- raster::res(dem)[1]
  oldw <- getOption("warn")
  options(warn = -1) #silence gdistance::transition warning for negative slopes
  message("Calculating slope")
  slope <- gdistance::transition(dem, .calc_slope, directions = neigh, symm=FALSE)
  options(warn = oldw)
  slope <- gdistance::geoCorrection(slope, scl = TRUE)
  adj <- gdistance::adjacencyFromTransition(slope)
  message("Calculating work")
  work <- slope
  work[adj] <- .calc_work(slope[adj], m, work_in_kcal = work_in_kcal)
  message("Calculating conductance (1 / work)")
  cond <- slope
  cond[adj] <- 1 / .calc_work(slope[adj], m)
  ans <- stack(dem,
               raster::raster(slope),
               raster::raster(work),
               raster::raster(cond))
  names(ans) <- c("DEM", "Slope", "Work", "Conductance")
  ans <- list(neighbors = neigh,
              mass = m,
              rasters = ans,
              cond_tr = cond)
  class(ans) <- "enerscape"
  rm(en_res, pos = 1) #remove global variable
  return(ans)
}
