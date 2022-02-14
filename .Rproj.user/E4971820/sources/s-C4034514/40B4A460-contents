#' Create spatial grid
#'
#' This creates a spatial grid from a rasterLayer (usually the DEM).
#' This grid is then used to run \code{enerscape()} on each grid
#' separately.
#' @param ext extent of x, x@extent
#' @param crs crs of x, x@crs
#' @param horizontal_grids integer, how many horizontal lines should the grid have.
#' @param vertical_grids integer, how many vertical lines should the grid have.
#' @return A named list with the grid (\code{grid}), the xy coordinates
#'  of its node points (\code{grid_xy}), horizontal (\code{horizontals}) and
#'  vertical (\code{verticals}) spatial SpatialLines, and adjustment value
#'  to get correct grid assignment (\code{adjust}). In most cases, you can ignore
#'  this list and just pass it to \code{enerscape_gridded()} and
#'  \code{enerscape_merge()} function as it is.
#' @examples
#' \dontrun{
#' library(raster)
#' library(enerscape)
#' dem <- getData("alt", path = tmpDir(), country = "austria")
#' dem <- projectRaster(dem, crs = crs("EPSG:31255"), res = c(1000, 1000))
# en_grid <- make_grid(extent(dem), crs(dem), 10, 10)
#' enerscape_gridded(dem, 10, grid = en_grid)
#' en <- enerscape_merge(tmpDir(), en_grid, dem)
#' }
#' @export
make_grid <- function(
  ext,
  crs,
  horizontal_grids,
  vertical_grids
) {
  # define region of interest
  roi <- sp::Polygon(
    matrix(c(ext@xmin, ext@ymin,
             ext@xmax, ext@ymin,
             ext@xmax, ext@ymax,
             ext@xmin, ext@ymax,
             ext@xmin, ext@ymin),
           byrow = TRUE, ncol = 2)
  )
  roi <- sp::SpatialPolygons(list(sp::Polygons(list(roi), ID = 1)),
                             proj4string = crs)
  # create gridlines
  grid <- sp::gridlines(roi,
                        easts = pretty(sp::bbox(roi)[1, ], n = horizontal_grids),
                        norths = pretty(sp::bbox(roi)[2, ], n = vertical_grids))
  # horizontal and vertical gridlines used for splitting
  horizontals <- grid[1]
  verticals <- grid[2]
  # node coordinates
  h <- sapply(seq_along(horizontals@lines[[1]]@Lines),
              function(i) {
                unique(horizontals@lines[[1]]@Lines[[i]]@coords[, 2])
              })
  v <- sapply(seq_along(verticals@lines[[1]]@Lines),
              function(i) {
                unique(verticals@lines[[1]]@Lines[[i]]@coords[, 1])
              })
  grid_xy <- expand.grid(pretty(sp::bbox(roi)[1, ], n = horizontal_grids),
                         pretty(sp::bbox(roi)[2, ], n = vertical_grids))
  # adjustment to get correct polygon angle nodes
  adjust <- length(unique(grid_xy[, 1]))
  ans <- list(grid = grid,
              grid_xy = grid_xy,
              horizontals = horizontals,
              verticals = verticals,
              adjust = adjust)
  class(ans) <- c("spatialGrid", "enerscape")
  return(ans)
}

#' Run enerscape for a SpatialGrid
#'
#' This runs enerscape for each block of a spatial grid sequentially.
#' @inherit enerscape
#' @param grid enerscape grid, as created using the \code{make_grid()} function.
#' @param writedir directory to write EnergyScapes for each grid; default to tmpDir().
#' @return NULL
#' @details \code{enerscape_gridded()} runs enerscape for each block of the grid
#'  sequentially and writes (only) the rasterLayer of the EnergyScape to the specified
#'  folder, default to the raster temporary directory. This temporary files are
#'  then merge back together by \code{enerscape_merge()}. This function also calculates
#'  enerscape for the vertical and horizontal strips used to split \code{dem} into blocks,
#'  which are then used to correct for artifacts of the splitting.
#' @examples
#' \dontrun{
#' library(raster)
#' library(enerscape)
#' dem <- getData("alt", path = tmpDir(), country = "austria")
#' dem <- projectRaster(dem, crs = crs("EPSG:31255"), res = c(1000, 1000))
#' en_grid <- make_grid(extent(dem), crs(dem), 10, 10)
#' enerscape_gridded(dem, 10, grid = en_grid)
#' en <- enerscape_merge(tmpDir(), en_grid, dem)
#' }
#' @export
enerscape_gridded <- function(
  dem,
  m,
  grid,
  unit = "joule",
  neigh = 16,
  method = "ARC",
  v = NULL,
  writedir = raster::tmpDir()
) {
  if (!all(c("spatialGrid", "enerscape") %in% class(grid))) {
    stop("grid is not an enerscape grid list. Create the grid using make_grid()")
  }
  message("single enerscape grids are written in ", writedir)
  # run enerscape for each polygon
  for (i in seq_len(nrow(grid$grid_xy))) {
    message(" - Running enerscape for grid ", i, " of ", nrow(grid$grid_xy))
    if (i %% grid$adjust == 0) {
      next
    }
    pts <- c(i, i + 1, i + grid$adjust, i + grid$adjust + 1)
    pts <- pts[c(1, 3, 4, 2)]
    if (max(pts) > nrow(grid$grid_xy)) {
      next
    }
    pol <- sp::SpatialPolygons(
      list(sp::Polygons(
        list(sp::Polygon(
          sp::SpatialPoints(grid$grid_xy[pts, ],
                            proj4string = grid$grid@proj4string)
        )), ID = 1
      )), proj4string = grid$grid@proj4string
    )
    sub_dem <- raster::crop(dem, pol)
    if (any(!is.na(raster::values(sub_dem)))) {
      en <- suppressMessages(enerscape(sub_dem, m, unit, neigh, method, v))
      raster::writeRaster(en$rasters$EnergyScape,
                          paste0(writedir, "enerscape-", i, ".tif"))
    }
  }
  # run enerscape for vertical corrections
  for (i in seq_along(grid$verticals@lines[[1]]@Lines)) {
    message(" - Calculating vertical correction ", i, " of ",
            length(grid$verticals@lines[[1]]@Lines))
    vert <- grid$verticals@lines[[1]]@Lines[i]
    vert <- raster::buffer(sp::SpatialLines(
      list(sp::Lines(
        list(sp::Line(
          vert[[1]]@coords)
        ), ID = 1)
      ),
      proj4string = grid$grid@proj4string
    ), raster::res(dem)[1] * 10)
    vert <- raster::crop(vert, dem)
    sub_dem <- raster::crop(dem, vert)
    if (any(!is.na(raster::values(sub_dem)))) {
      sub_dem <- raster::trim(raster::crop(sub_dem, vert))
      en <- suppressMessages(enerscape(sub_dem, m, unit, neigh, method, v))
      raster::writeRaster(en$rasters$EnergyScape,
                          paste0(writedir, "enerscape_vertical-", i, ".tif"))
    }
  }
  # run enerscape for horizontal corrections
  for (i in seq_along(grid$horizontals@lines[[1]]@Lines)) {
    message(" - Calculating horizontal correction ", i, " of ",
            length(grid$horizontals@lines[[1]]@Lines))
    hor <- grid$horizontals@lines[[1]]@Lines[i]
    hor <- raster::buffer(sp::SpatialLines(
      list(sp::Lines(
        list(sp::Line(
          hor[[1]]@coords)
        ), ID = 1)
      ),
      proj4string = grid$grid@proj4string
    ), raster::res(dem)[1] * 10)
    hor <- raster::crop(hor, dem)
    sub_dem <- raster::crop(dem, hor)
    if (any(!is.na(raster::values(sub_dem)))) {
      sub_dem <- raster::trim(raster::crop(sub_dem, hor))
      en <- suppressMessages(enerscape(sub_dem, m, unit, neigh, method, v))
      raster::writeRaster(en$rasters$EnergyScape,
                          paste0(writedir, "enerscape_horizontal-", i, ".tif"))
    }
  }
}

#' Merge together EnergyScapes for a gridded enerscape
#'
#' This merge back together EnergyScapes computed using \code{enerscape_gridded()}
#' correcting for artifacts generated by the splitting.
#' @inherit enerscape_gridded
#' @return rasterLayer of the merged and corrected EnergyScapes.
#' @examples
#' \dontrun{
#' library(raster)
#' library(enerscape)
#' dem <- getData("alt", path = tmpDir(), country = "austria")
#' dem <- projectRaster(dem, crs = crs("EPSG:31255"), res = c(1000, 1000))
#' en_grid <- make_grid(extent(dem), crs(dem), 10, 10)
#' enerscape_gridded(dem, 10, grid = en_grid)
#' en <- enerscape_merge(tmpDir(), en_grid, dem)
#' }
#' @export
enerscape_merge <- function(
  writedir = raster::tmpDir(),
  grid,
  dem
) {
  grids <- list.files(writedir, pattern = "enerscape-", full.names = TRUE)
  verts <- list.files(writedir, "enerscape_vertical-", full.names = TRUE)
  hors <- list.files(writedir, "enerscape_horizontal-", full.names = TRUE)
  template <- raster::raster(raster::extent(grid$grid),
                             res = raster::res(raster::raster(grids[1])),
                             crs = grid$grid@proj4string)
  # add all enerscape grids
  message(" - Merging enerscape grids together")
  for (x in grids) {
    r <- raster::raster(x)
    template <- raster::merge(template, r)
  }
  # correct vertical artifacts
  message(" - Correcting vertical artifacts")
  for (v in verts) {
    vert <- raster::raster(v)
    correction_xy <- raster::xyFromCell(vert, seq_len(raster::ncell(vert)))
    correction_xy <- correction_xy[correction_xy[, 1] > min(correction_xy[, 1]), ]
    correction_xy <- correction_xy[correction_xy[, 1] > min(correction_xy[, 1]), ]
    correction_xy <- correction_xy[correction_xy[, 1] < max(correction_xy[, 1]), ]
    correction_xy <- correction_xy[correction_xy[, 1] < max(correction_xy[, 1]), ]
    correction <- raster::extract(vert, correction_xy)[[1]]
    cells <- raster::cellFromXY(template, correction_xy)
    cells <- cells[!is.nan(cells)]
    vals <- raster::cellFromXY(vert, correction_xy)
    if (length(cells) == length(vals)) {
      raster::values(template)[cells] <- raster::values(vert)[vals]
    } else {
      stop("Vector to replace is not the same length - stopping")
    }
  }
  # correct horizontal artifacts
  message(" - Correcting horizontal artifacts")
  for (h in hors) {
    hor <- raster::raster(h)
    correction_xy <- raster::xyFromCell(hor, seq_len(raster::ncell(hor)))
    correction_xy <- correction_xy[correction_xy[, 2] > min(correction_xy[, 2]), ]
    correction_xy <- correction_xy[correction_xy[, 2] > min(correction_xy[, 2]), ]
    correction_xy <- correction_xy[correction_xy[, 2] < max(correction_xy[, 2]), ]
    correction_xy <- correction_xy[correction_xy[, 2] < max(correction_xy[, 2]), ]
    correction <- raster::extract(hor, correction_xy)[[1]]
    cells <- raster::cellFromXY(template, correction_xy)
    cells <- cells[!is.nan(cells)]
    vals <- raster::cellFromXY(hor, correction_xy)
    if (length(cells) == length(vals)) {
      raster::values(template)[cells] <- raster::values(hor)[vals]
    } else {
      stop("Vector to replace is not the same length - stopping")
    }
  }
  template <- raster::mask(template, dem)
  return(template)
}
