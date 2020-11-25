#' Calculate shortest path between origin and destination
#' @inherit .calc_slope
#' @inherit .calc_work
#' @inherit enerscape
#' @param en an enerscape object obtained with enerscape::enerscape().
#' @param or origin point.
#' @param dest destination point.
#' @param simulate_random_points if to simulate shortest path among random
#'   points. default = FALSE.
#' @param rep number or random origin and destination points if
#'   simulate_random_points = TRUE. default = 10.
en_shortest_path <- function(
  en,
  neigh = 4,
  or,
  dest,
  simulate_random_points = FALSE,
  rep = 10
) {
  if (class(en) != "enerscape") {
    stop("en must be an enerscape object")
  } else {
    x <- en$rasters$DEM
    work <- en$rasters$Work
    cond <- en$cond_tr
  }
  ans <- list() #initialize return value
  if (simulate_random_points) { #random points
    for (i in seq_len(rep)) {
      p <- raster::xyFromCell(x, sample(raster::ncell(x), 2))
      while (any(is.na(raster::extract(x, p)))) {
        p <- raster::xyFromCell(x, sample(raster::ncell(x), 2))
      }
      shortest <- gdistance::shortestPath(cond, p[1, ], p[2, ], output = "SpatialLines")
      cost <- raster::extract(work, shortest)
      cost <- sum(cost[[1]], na.rm = TRUE)
      ans[["Costs"]] <- rbind(ans[["Costs"]], cost)
      ans[["Origins"]] <- rbind(ans[["Origins"]], p[1, ])
      ans[["Destinations"]] <- rbind(ans[["Destinations"]], p[2, ])
      ans[["Paths"]][[i]] <- shortest
    }
    ans[["Paths"]] <- do.call(rbind, ans[["Paths"]])
    ans[["Distances"]] <- sp::SpatialLinesLengths(ans[["Paths"]])
    ans[["Costs"]] <- as.numeric(ans[["Costs"]])
    raster::plot(x, main = paste0("Shortest paths between ", rep, " random points"))
    graphics::points(ans[["Origins"]],
                     pch = 20,
                     col = grDevices::adjustcolor("blue", alpha.f = 0.75))
    graphics::points(ans[["Destinations"]],
                     pch = 20,
                     col = grDevices::adjustcolor("grey20", alpha.f = 0.75))
    graphics::lines(ans[["Paths"]],
                    lt = 2, col = grDevices::adjustcolor("grey10", alpha.f = 0.75))
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
    shortest <- gdistance::shortestPath(cond, p[1, ], p[2, ], output = "SpatialLines")
    cost <- raster::extract(work, shortest)
    cost <- sum(cost[[1]], na.rm = TRUE)
    ans[["shortest path"]] <- list(Cost = cost,
                                   Path = shortest,
                                   "Distance" = sp::SpatialLinesLengths(shortest))
    raster::plot(x, main = "Shortest paths between two points")
    graphics::points(p[1, 1], p[1, 2], pch = 20, col = grDevices::adjustcolor("blue", alpha.f = 0.75))
    graphics::points(p[2, 1], p[2, 2], pch = 20, col = grDevices::adjustcolor("grey20", alpha.f = 0.75))
    graphics::lines(shortest, lt = 2, col = grDevices::adjustcolor("grey10", alpha.f = 0.75))
    return(ans)
  }
}
