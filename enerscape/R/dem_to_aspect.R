#'
#'
#'
#'
#'
gradient <- function(dem, origin, destination) {
  if (class(origin) != "data.frame" | class(destination) != "data.frame") {
    stop("origin and destination should be data.frames with x, y coordinates")
  }
  asp <- raster::terrain(dem, "aspect", neighbors = 4, unit = "degrees")
  x <- destination$x - origin$x
  y <- destination$y - origin$y
  alpha <- asin(y / sqrt(y ^ 2 + x ^ 2)) * 180 / pi
  asp <- asp - 90 + alpha
  asp[asp <= 90 | asp >= 270] <- -1
  asp[asp > 90 & asp < 270] <- 1
  return(asp)
}

library(gdistance)
library(raster)
source("R/dem_to_slope.R")
source("R/calc_work.R")
dem <- raster("../../enerscape-paper/data/dem-abruzzi.tif")
dem <- aggregate(dem, 10)
p <- dismo::randomPoints(dem, 2)
or <- data.frame(x = p[1, 1], y = p[1, 2])
dest <- data.frame(x = p[2, 1], y = p[2, 2])
slope <- dem_to_slope(dem)
grad <- gradient(dem, or, dest)
grad <- grad * slope

plot(grad, col = RColorBrewer::brewer.pal(9, "RdBu"))
points(p, pch = 20)

w <- calc_work(slope, 1)
s <- calc_work(grad, 1)
plot(w)
plot(s)

plot(dem, topo.colors(100))
plot(sign(grad), add = TRUE,
     col = c(adjustcolor("white", alpha.f = 0.0),
             adjustcolor("red", alpha.f = 0.2)))
points(or, pch = 20, col = "blue")
points(dest, pch = 20, col = "black")
cond <- 1 / w
lcp <- transition(cond, function(x) min(x), 16)
shortest <- shortestPath(lcp, as.numeric(or), as.numeric(dest), output = "SpatialLines")
lines(shortest, lt = 2)
cond <- 1 / s
lcp <- transition(cond, function(x) min(x), 16)
shortest <- shortestPath(lcp, as.numeric(or), as.numeric(dest), output = "SpatialLines")
lines(shortest)

