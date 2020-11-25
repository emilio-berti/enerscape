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
  if (!unit %in% c("joule", "kcal")) {
    stop("unit must be one of 'joule' or 'kcal'")
  }
  work_in_kcal <- ifelse(unit == "joule", FALSE, TRUE)
  message("Calculating slope")
  slope <- dem_to_slope(dem, neigh = neigh)
  message("Calculating work")
  work <- calc_work(slope, m, work_in_kcal = work_in_kcal)
  ans <- raster::stack(c(dem, slope, work))
  names(ans) <- c("dem", "slope", "work")
  return(ans)
}
