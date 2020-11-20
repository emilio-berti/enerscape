library(raster)

dem <- raster::raster("../../enerscape-paper/data/dem-abruzzi.tif")
dem <- aggregate(dem, 10)
mat <- as.matrix(dem)

origin <- data.frame(x = 863047, y = 4667429)
dest <- data.frame(x = 872700, y = 4693600)



grad <- function(mat, origin, destination) {
  ans <- mat
  origin_col <- colFromX(dem, origin$x)
  origin_row <- rowFromY(dem, origin$y)
  dest_col <- colFromX(dem, dest$x)
  dest_row <- rowFromY(dem, dest$y)
  ans[origin_row, origin_col] <- NA
  x <- origin_row
  y <- origin_col
  neigh_grad(mat, x, y)
}

neigh_grad <- function(mat, x, y) {
  center <- mat[x, y]
  up <- mat[x + 1, y]
  up <- sign(up - center)
  down <- mat[x - 1, y]
  down <- sign(down - center)
  right <- mat[x, y + 1]
  right <- sign(right - center)
  left <- mat[x, y - 1]
  left <- sign(left - center)
  return(list = c("center" = center,
                  "left" = left,
                  "right" = right,
                  "up" = up,
                  "down" = down))
}
