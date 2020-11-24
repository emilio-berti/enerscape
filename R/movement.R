#' Internal function for en_shortest_path - calculate slope
#' @param x a digital elevation model raster layer
.calc_slope <- function(x){
  diff <- x[2] - x[1]
  ipo <- sqrt(en_res ^ 2 + diff ^ 2)
  alpha <- asin(diff / ipo) * 180 / pi
  return(alpha)
}
#' Internal function for en_shortest_path - calculate work
#' @param x a slope transition layer
#' @inherit calc_work
.calc_work <- function(
  x,
  m,
  J_to_kcal = 4184, #Joules to kilocalories
  work_in_kcal = TRUE){
  if (!all(is.na(x))) {
    work <- 8 * m ^ (-0.34) + 100 * (1 + sin((2 * x - 74) / 180 * pi)) * m ^ (-0.12)
  } else {
    work <- 8 * m ^ (-0.34)
  }
  work <- m * work * en_res
  if (work_in_kcal) {
    work <- work / J_to_kcal
  }
  return(work)
}
#' Calculate shortest path between origin and destination
#' @inherit .calc_slope
#' @inherit .calc_work
#' @param neigh number of neighbours cells to consider
#' @param or origin point
#' @param dest destination point
#' @param simulate_random_points if to simulate shortest path among random
#'   points. default = FALSE
#' @param rep number or random origin and destination points if
#'   simulate_random_points = TRUE. default = 10
en_shortest_path <- function(
  x,
  m,
  neigh = 4,
  or,
  dest,
  simulate_random_points = FALSE,
  rep = 10
) {
  ans <- list() #initialize return value
  # transition layers cannot accept optional arguments. The resolution is saved
  # as global variable and deleted before return. Deletion takes place in the
  # parent environment of the function.
  if ("en_res" %in% ls()) {
    stop("You have a variable called `en_res`\n
         Please rename it as something else")
  }
  en_res <<- raster::res(x)[1]
  oldw <- getOption("warn")
  options(warn = -1)
  slope <- gdistance::transition(x, .calc_slope, neigh, symm=FALSE)
  options(warn = oldw)
  slope <- gdistance::geoCorrection(slope, scl = TRUE)
  adj <- gdistance::adjacencyFromTransition(slope)
  work <- slope
  cond <- slope
  work[adj] <- .calc_work(slope[adj], m)
  cond[adj] <- 1 / .calc_work(slope[adj], m)
  ans[["rasters"]] <- list(DEM = x,
                           Slope = raster::raster(slope),
                           Work = raster::raster(work),
                           Conductance = raster::raster(cond))
  # random points --------------
  if (simulate_random_points) {
    ans[["sims"]] <- list()
    for (i in seq_len(rep)) {
      p <- raster::xyFromCell(x, sample(raster::ncell(x), 2))
      while (any(is.na(raster::extract(x, p)))) {
        p <- raster::xyFromCell(x, sample(raster::ncell(x), 2))
      }
      shortest <- gdistance::shortestPath(cond, p[1, ], p[2, ], output = "SpatialLines")
      cost <- raster::extract(raster::raster(work), shortest)
      cost <- sum(cost[[1]], na.rm = TRUE)
      ans[["sims"]][["Costs"]] <- rbind(ans[["sims"]][["Costs"]], cost)
      ans[["sims"]][["Origins"]] <- rbind(ans[["sims"]][["Origins"]], p[1, ])
      ans[["sims"]][["Destinations"]] <- rbind(ans[["sims"]][["Destinations"]], p[2, ])
      ans[["sims"]][["Paths"]][[i]] <- shortest
    }
    ans[["sims"]][["Paths"]] <- do.call(rbind, ans[["sims"]][["Paths"]])
    raster::plot(x, main = paste0("Shortest paths between ", rep, " random points"))
    graphics::points(ans[["sims"]][["Origins"]],
                     pch = 20,
                     col = grDevices::adjustcolor("blue", alpha.f = 0.75))
    graphics::points(ans[["sims"]][["Destinations"]],
                     pch = 20,
                     col = grDevices::adjustcolor("grey20", alpha.f = 0.75))
    graphics::lines(ans[["sims"]][["Paths"]],
                    lt = 2, col = grDevices::adjustcolor("grey10", alpha.f = 0.75))
    rm(en_res, pos = 1)
    return(ans)
  } else {
    p <- rbind(or, dest)
    if (class(p)[1] == "data.frame") {
      p <- as.matrix(p)
    } else if (class(p)[1] != "matrix") {
      stop("Origin and destination must be data.frames, matrices, or vectors")
    }
    if (any(names(p) != c("x", "y"))) {
      names(p) <- c("x", "y")
    }
    # work_mat = as.matrix(transitionMatrix(work))
    shortest <- gdistance::shortestPath(cond, p[1, ], p[2, ], output = "SpatialLines")
    # cost
    cost <- raster::extract(raster::raster(work), shortest)
    cost <- sum(cost[[1]], na.rm = TRUE)
    ans[["shortest path"]] <- list(Cost = cost, Path = shortest)
    raster::plot(x, main = "Shortest paths between two points")
    graphics::points(p[1, 1], p[1, 2], pch = 20, col = grDevices::adjustcolor("blue", alpha.f = 0.75))
    graphics::points(p[2, 1], p[2, 2], pch = 20, col = grDevices::adjustcolor("grey20", alpha.f = 0.75))
    graphics::lines(shortest, lt = 2, col = grDevices::adjustcolor("grey10", alpha.f = 0.75))
    rm(en_res, pos = 1)
    return(ans)
  }
}
