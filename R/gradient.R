#' Rotate aspect raster to orient it northward
#' @param x raster of terrain aspect
#' @param angle angle of rotation, 90 for North = 90 degrees
.rotate <- function(x, angle = 90) {
  ans <- x + 90
  ans[ans >= 360] <- ans[ans >= 360] - 360
  return(ans)
}
#' Flip aspect terrain horizontally, i.e. East to West and West to East
#' @inherit .rotate
.flip <- function(x) {
  ans <- 360 - x
  ans[ans >= 360] <- ans[ans >= 360] - 360
  return(ans)
}
#' Calculate the gradient relative to the minimum distance between two points
#' @param dem digital elevation model raster
#' @param origin data.frame with x and y coordinates of origin point
#' @param destination data.frame with x and y coordinates of destination point
#' @details This function return the gradient of the slope evaluated considering
#'   movement always parallel to the minimum distance axis between the origin
#'   and destination points
gradient <- function(dem, origin, destination) {
  if (class(origin) != "data.frame" | class(destination) != "data.frame") {
    stop("origin and destination should be data.frames with x, y coordinates")
  }
  asp <- raster::terrain(dem, "aspect", neighbors = 4, unit = "degrees")
  asp <- .rotate(asp)
  asp <- .flip(asp)
  x <- destination$x - origin$x
  y <- destination$y - origin$y
  if (x >= 0 & y >= 0) {
    alpha <- asin(y / sqrt(y ^ 2 + x ^ 2)) * 180 / pi
  } else if (x >= 0 & y < 0) {
    alpha <- 360 + asin(y / sqrt(y ^ 2 + x ^ 2)) * 180 / pi
  } else if (x < 0 & y >= 0) {
    alpha <- 180 - asin(y / sqrt(y ^ 2 + x ^ 2)) * 180 / pi
  } else if (x < 0 & y < 0) {
    alpha <- 180 - asin(y / sqrt(y ^ 2 + x ^ 2)) * 180 / pi
  } else {
    stop("Something wrong with angles - this should not happen, please contact support")
  }
  if (alpha < 0) {
    alpha <- 360 - alpha
  }
  alpha_min <- alpha - 90
  if (alpha_min < 0) {
    alpha_min <- 360 - alpha_min
  } else if (alpha_min >= 360) {
    alpha_min1 <- alpha_min - 360
  }
  alpha_max <- alpha + 90
  if (alpha_max < 0) {
    alpha_max <- 360 - alpha_max
  } else if (alpha_max >= 360) {
    alpha_max <- alpha_max - 360
  }
  if (alpha_min < alpha_max) {
    asp[asp <= alpha_min | asp >= alpha_max] <- -1
  } else if (alpha_min > alpha_max) {
    asp[asp <= alpha_min & asp >= alpha_max] <- -1
  }
  asp[asp != -1] <- 1
  # ext <- extent(rbind(origin, destination))
  # asp <- crop(asp, ext)
  # asp <- projectRaster(asp, dem)
  # asp[is.na(asp)] <- 1
  return(asp)
}
