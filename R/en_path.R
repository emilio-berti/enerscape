#' Compute the energy costs for a chosen path
#'
#' This returns the distance and energy costs of traveling a chosen path.
#' Optionally, the path can be selected by specifying the number of nodes and
#' clicking on the plot.
#' @param en an enerscape object.
#' @param p path as SpatialLines.
#' @param draw if TRUE the path will be chosen by drawing it on the map/
#' @param n number of node points for the path.
#' @param plot if TRUE plot the path
#' @return A list with elements the path, its travel distance and energy costs.
#' @examples
#' \donttest{
#' library(raster)
#' library(enerscape)
#' data("volcano")
#' dem <- raster(volcano)
#' en <- enerscape(dem, 10, unit = "kcal", neigh = 16)
# p <- coordinates(en$rasters$DEM)[sample(1:ncell(en$rasters$DEM), 5), ]
# path <- sp::SpatialLines(list(sp::Lines(sp::Line(p), ID = "path")))
# en_path(en, path)
#' }
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
en_path <- function(
  en,
  p = NULL,
  draw = FALSE,
  n = NULL,
  plot = TRUE
) {
  if (is.null(p)) {
    if (draw & is.null(n)) {
      stop("Enter number of points to draw the path")
    } else if (draw & !is.null(n)) {
      graphics::par(mfrow = c(1, 1))
      raster::plot(en$rasters$DEM)
      p <- raster::click(n = n)
      p <- matrix(p, ncol = 2, byrow = TRUE)
    } else {
      stop("Specify the path or draw it (draw = TRUE)")
    }
    p <- sp::SpatialLines(list(sp::Lines(sp::Line(p), 1)))
  } else if (class(p)[1] != "SpatialLines") {
    stop("Path must be a SpatialLines object")
  }
  w <- raster::extract(en$rasters$EnergyScape, p)[[1]]
  ans <- list(Path = p,
              Distance = sp::SpatialLinesLengths(p),
              Cost = sum(w))
  if (plot) {
    raster::plot(en$rasters$DEM)
    graphics::lines(p)
  }
  return(ans)
}
