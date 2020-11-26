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
  message("Calculating slope")
  slope <- gdistance::transition(dem, .calc_slope,
                                 directions = neigh,
                                 symm = FALSE)
  adj <- gdistance::adjacencyFromTransition(slope)
  message("Calculating work")
  work <- slope
  work[adj] <- .calc_work(slope[adj], m, work_in_kcal = work_in_kcal)
  message("Calculating conductance (1 / work)")
  cond <- slope
  cond[adj] <- 1 / .calc_cond(slope[adj], m)
  # transition matrices are ok, but conversion to rasters introduces NAs for
  # exactly zero inclines. Correcting manually.
  s <- raster::raster(slope)
  s[is.na(s)] <- 0
  w <- raster::raster(work)
  w[is.na(w)] <- 8 * m ^ (-0.34) + 100 * (1 + sin((-74) / 180 * pi)) * m ^ (-0.12) * m * en_res
  con <- raster::raster(cond)
  con[is.na(con)] <- 1 / 8 * m ^ (-0.34) + 100 * (1 + sin((-74) / 180 * pi)) * m ^ (-0.12) * m * en_res
  ans <- stack(dem, s, w, con)
  names(ans) <- c("DEM", "Slope", "Work", "Conductance")
  ans <- list(neighbors = neigh,
              mass = m,
              rasters = ans,
              cond_tr = cond)
  class(ans) <- "enerscape"
  rm(en_res, pos = 1) #remove global variable
  message("  -------------- enerscape ---------------
  do not use slope for distance calculations
  as it has negative values (see below).")
  return(ans)
}
