plot_enerscape_terra <- function(
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
                main = "DEM")
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
                main = "slope")
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
                main = "work")
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
