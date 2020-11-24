#' Calculate the energy landscape
#'
#' @inherit dem_to_slope
#' @inherit calc_work
#' @param units if Joules ("J") or kilocalories ("kcal)

enerscape <- function(
  dem,
  m,
  units = "J"
) {
  message("Calculating slope")
  slope <- raster::terrain(dem, "slope")
  message("Calculating work")
  if (units == "J"){
    work <- calc_work(slope, m)
  } else if (units == "kcal") {
    work <- calc_work(slope, m, work_in_kcal = TRUE)
  } else {
    stop("Units not J or kcal.")
  }
  if (class(dem) == "character") {
    dem <- raster::raster(dem)
    ans <- raster::stack(c(dem, slope, work))
  } else if (class(dem) == "RasterLayer") {
    ans <- raster::stack(c(dem, slope, work))
  } else {
    ans <- c(dem, slope, work)
  }
  names(ans) <- c("dem", "slope", "work")
  return(ans)
}
