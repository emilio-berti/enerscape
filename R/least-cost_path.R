#' Compute least-cost paths
#'
#' Calculate the least-cost path (lcp) between origin and destination
#' @param en an enerscape object obtained with \code{enerscape()}.
#' @param or origin point.
#' @param dest destination point.
#' @param simulate_random_points if to simulate least-cost path among random
#'   points. default = FALSE.
#' @param rep number or random origin and destination points if
#'   \code{simulate_random_points = TRUE}. default = 10.
#' @param plot if to plot the output.
#' @return A list with point locations, least-cost path as SpatialLines, energy
#'   costs and distances.
#' @details If \code{or} and \code{dest} are not specified, the least-cost path
#'   is specified by setting \code{simulate_random_points = TRUE} and \code{rep}
#'   equal to the number of random paths to compute.
#' @examples
#' library(raster)
#' data("volcano")
#' dem <- raster(volcano)
#' en <- enerscape(dem, 10, unit = "kcal", neigh = 16)
#' p <- xyFromCell(dem, sample(ncell(dem), 2))
#' lcp <- en_lcp(en, or = p[1, ], dest = p[2, ])
en_lcp <- function(
  en,
  or,
  dest,
  simulate_random_points = FALSE,
  rep = 10,
  plot = TRUE
) {
  if (class(en) != "enerscape") {
    stop("en must be an enerscape object")
  } else {
    x <- en$rasters$DEM
    work <- en$rasters$EnergyScape
    cond <- en$cond_tr
  }
  ans <- list() #initialize return value
  if (simulate_random_points) { #random points
    ans[["Paths"]] <- list()
    for (i in seq_len(rep)) {
      message(i, " of ", rep, " random points ...")
      p <- raster::xyFromCell(x, sample(raster::ncell(x), 2))
      while (any(is.na(raster::extract(x, p)))) {
        p <- raster::xyFromCell(x, sample(raster::ncell(x), 2))
      }
      lcp <- gdistance::shortestPath(cond, p[1, ], p[2, ],
                                     output = "SpatialLines")
      cost <- raster::extract(work, lcp)
      cost <- sum(cost[[1]], na.rm = TRUE)
      ans[["Origins"]] <- rbind(ans[["Origins"]], p[1, ])
      ans[["Destinations"]] <- rbind(ans[["Destinations"]], p[2, ])
      ans[["Paths"]][[i]] <- lcp
      ans[["Costs"]] <- rbind(ans[["Costs"]], cost)
    }
    ans[["Paths"]] <- do.call(rbind, ans[["Paths"]])
    ans[["Distances"]] <- sp::SpatialLinesLengths(ans[["Paths"]])
    ans[["Costs"]] <- as.numeric(ans[["Costs"]])
    if (plot == TRUE) {
      raster::plot(x, main = paste0("Least-cost paths between ", rep,
                                    " random points"))
      graphics::points(ans[["Origins"]], pch = 20,
                       col = grDevices::adjustcolor("blue", alpha.f = 0.75))
      graphics::points(ans[["Destinations"]], pch = 20,
                       col = grDevices::adjustcolor("grey20", alpha.f = 0.75))
      graphics::lines(ans[["Paths"]], lt = 2,
                      col = grDevices::adjustcolor("grey10", alpha.f = 0.75))
    }
    return(ans)
  } else { #with defined coordinates
    p <- rbind(or, dest)
    if (class(p)[1] == "data.frame") {
      p <- as.matrix(p)
    } else if (class(p)[1] != "matrix") {
      stop("Origin and destination must be data.frames, matrices, or vectors")
    }
    if (any(names(p) != c("x", "y"))) {
      names(p) <- c("x", "y")
    }
    lcp <- gdistance::shortestPath(cond, p[1, ], p[2, ],
                                   output = "SpatialLines")
    cost <- raster::extract(work, lcp)
    cost <- sum(cost[[1]], na.rm = TRUE)
    ans <- list(Origin = p[1, ],
                Destination = p[2, ],
                Path = lcp,
                Cost = cost,
                Distance = sp::SpatialLinesLengths(lcp))
    if (plot == TRUE) {
      raster::plot(x, main = "Least-cost paths between two points")
      graphics::points(p[1, 1], p[1, 2], pch = 20,
                       col = grDevices::adjustcolor("blue", alpha.f = 0.75))
      graphics::points(p[2, 1], p[2, 2], pch = 20,
                       col = grDevices::adjustcolor("grey20", alpha.f = 0.75))
      graphics::lines(lcp, lt = 2,
                      col = grDevices::adjustcolor("grey10", alpha.f = 0.75))
    }
    return(ans)
  }
}
