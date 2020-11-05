#' @param enerscape is the output of the enerscape() function
#' @param what specifies what to plot. Available are "work", "slope", or "all"
#' @param contour specifies if DEM contour plots should be overlayed on the plot
#' @param n_contour specifies how many levels of DEM contours should be plotted,
#'   if any
#' @param axes specifies if axes should be added to the plot
plot_enerscape <- function(
  en,
  what = "all",
  contour = TRUE,
  n_contour = 10,
  axes = FALSE
  ) {
  if (what != "all") {
    terra::plot(en[what], col = topo.colors(9), axes = axes, main = what)
    if (contour) {
      terra::contour(en$dem, add = TRUE, nlevels = n_contour, col = "grey10", lw = 2)
    }
  } else {
    par(mfrow = c(1, 2))
    terra::plot(en$slope, col = topo.colors(9), axes = axes, main = "slope")
    if (contour) {
      terra::contour(en$dem, add = TRUE, nlevels = n_contour, col = "grey10", lw = 2)
    }
    terra::plot(en$work, col = topo.colors(9), axes = axes, main = "work")
    if (contour) {
      terra::contour(en$dem, add = TRUE, nlevels = n_contour, col = "grey10", lw = 2)
    }
    par(mfrow = c(1, 1))
  }
}
