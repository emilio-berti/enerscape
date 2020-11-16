#' @param enerscape is the output of the enerscape() function
#' @param what specifies what to plot. Available are "work", "slope", or "all"
#' @param contour specifies if DEM contour plots should be overlayed on the plot
#' @param n_contour specifies how many levels of DEM contours should be plotted,
#'   if any
#' @param axes specifies if axes should be added to the plot
#' @param max_quantile specifies the maximum quantile to be displayed, i.e.
#'   values above this quantile are re-assigned to the maximum value within the
#'   quantile. This is useful for plotting, as sometimes few cells have
#'   extremely high values, due to almost vertical displacement. In such cases,
#'   a max_quantile = 0.99 removes these outliers.
plot_enerscape <- function(
  en,
  what = "all",
  contour = TRUE,
  n_contour = 10,
  axes = FALSE,
  max_quantile = 1
) {
  if (what != "all") {
    p <- en[what]
    if (max_quantile != 1){
      v <- values(p)
      v <- v[!is.na(v)]
      p[p > quantile(v, max_quantile)] <- quantile(v, max_quantile)
    }
    terra::plot(p,
                col = topo.colors(100),
                axes = axes,
                main = what)
    if (contour) {
      terra::contour(en$dem,
                     add = TRUE,
                     nlevels = n_contour,
                     col = "grey10",
                     lw = 2)
    }
  } else {
    par(mfrow = c(1, 3))
    p <- en$dem
    if (max_quantile != 1){
      v <- values(p)
      v <- v[!is.na(v)]
      p[p > quantile(v, max_quantile)] <- quantile(v, max_quantile)
    }
    terra::plot(p,
                col = terrain.colors(100),
                axes = axes,
                main = "DEM",
                pal = list(shrink = 0.7))
    if (contour) {
      terra::contour(en$dem,
                     add = TRUE,
                     nlevels = n_contour,
                     col = "grey10",
                     lw = 2)
    }
    p <- en$slope
    if (max_quantile != 1){
      v <- values(p)
      v <- v[!is.na(v)]
      p[p > quantile(v, max_quantile)] <- quantile(v, max_quantile)
    }
    terra::plot(p,
                col = topo.colors(100),
                axes = axes,
                main = "slope",
                pal = list(shrink = 0.7))
    if (contour) {
      terra::contour(en$dem,
                     add = TRUE,
                     nlevels = n_contour,
                     col = "grey10",
                     lw = 2)
    }
    p <- en$work
    if (max_quantile != 1){
      v <- values(p)
      v <- v[!is.na(v)]
      p[p > quantile(v, max_quantile)] <- quantile(v, max_quantile)
    }
    terra::plot(p,
                col = topo.colors(100),
                axes = axes,
                main = "work",
                pal = list(shrink = 0.7))
    if (contour) {
      terra::contour(en$dem,
                     add = TRUE,
                     nlevels = n_contour,
                     col = "grey10",
                     lw = 2)
    }
    par(mfrow = c(1, 1))
  }
}
