#' @title Create a gridPolygon
#'
#' @param mat matrix with coordinates.
#' @param proj4 string, proj4string of the original raster.
#' @param ID integer, ID of the polygon (can be anything as lons as unique).
#'
#' @return a SpatialPolygonsDataFrame.
#'
#' @details gridPolygon() is used within gridRaster() and is mostly for enerscape
#' package internal functioning.
#'
#' @examples
#' mat <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
#' proj4 <- "+proj=longlat +datum=WGS84 +no_defs"
#' gridPolygon(mat, proj4, ID = 1)
gridPolygon <- function(
  mat,
  proj4,
  ID
) {
  pol <- sp::SpatialPolygons(
    list(
      sp::Polygons(
        list(
          sp::Polygon(
            mat
          )
        ),
        ID = 1
      )
    ),
    proj4string = sp::CRS(proj4)
  )
  pol <- sp::SpatialPolygonsDataFrame(pol, data = data.frame(ID = ID))
  return(pol)
}
#' @title Create spatial grid
#'
#' @param r RasterLayer.
#' @param split.hor integer, number of horizontal splits.
#' @param split.vert integer, number of vertical splits.
#'
#' @return a SpatialPolyonsDataFrame.
#'
#' @details gridRaster makes a spatial grid that will be used to divide
#' the raster into smaller units for computations. The final grid may not
#' necessarily be split into split.hor times split.vert blocks, but it can happen
#' that the final grid has (split.hor - 1) times (split.vert - 1) blocks.
#'
#' @examples
#' \donttest{
#' library(raster)
#' data("volcano")
#' r <- raster(volcano)
#' crs(r) <- "+proj=longlat +datum=WGS84 +no_defs"
#' gridRaster(r, split.hor = 3, split.vert = 3)
#' }
gridRaster <- function(
  r,
  split.hor,
  split.vert
) {
  # get proj4string of the CRS
  proj4 <- sp::proj4string(r)
  # get corner points
  x <- seq(raster::xmin(r), raster::xmax(r), length.out = split.hor)
  y <- seq(raster::ymin(r), raster::ymax(r), length.out = split.vert)
  # create polygons
  pols <- list()
  grids <- c()
  id <- 0
  for (i in seq_len(split.hor - 1)) {
    for (j in seq_len(split.vert - 1)) {
      id <- id + 1
      mat <- matrix(c(
        x[i], y[j],
        x[i + 1], y[j],
        x[i + 1], y[j + 1],
        x[i], y[j + 1],
        x[i], y[j]
      ),
      ncol = 2,
      byrow = TRUE)
      # create the polygon for this coordinates
      pol <- gridPolygon(mat, proj4, ID = id)
      # create grids or append the polygon to it if it exists already
      if (length(grids) == 0) {
        grids <- pol
      } else {
        grids <- rbind(grids, pol)
      }
    }
  }
  return(grids)
}

#' @title Split raster into blocks
#'
#' @param x RasterLayer.
#' @param y SpatialPolygonsDataFrame, polygon grid (as created using
#'   gridRaster()),
#' @param buffer TRUE/FALSE, if to create a small buffer around the polygon to
#'   avoid missing values at the polygon border when merging back together.
#' @param write TRUE/FALSE, if to write the raster to disk.
#' @param out.dir string, output directory; default to tempdir().
#'
#' @details as splitting exactly at the spatialPolygons boundaries will create
#' later artifacts at the polygons edge, it is better to always set
#' buffer = TRUE, which will polygons buffered by twice the
#' resolution of the raster to split before cropping.
#'
#' @return an enerscape rasterBlocks objects, which is a list of RasterLayers.
#'
#' @examples
#' \donttest{
#' library(raster)
#' data("volcano")
#' r <- raster(volcano)
#' crs(r) <- "+proj=longlat +datum=WGS84 +no_defs"
#' gr <- gridRaster(r, split.hor = 3, split.vert = 3)
#' splits <- splitRaster(r, gr)
#' }
splitRaster <- function(
  x,
  y,
  buffer = TRUE,
  write = FALSE,
  out.dir = tempdir()
) {
  if (!write) ans <- list()
  for (i in seq_along(y)) {
    pol <- y[i, ]
    if (buffer) pol <- raster::buffer(pol, max(raster::res(x)) * 2)
    cropped <- raster::crop(x, pol)
    if (write){
      raster::writeRaster(cropped, paste0(out.dir, "/grid-", i, ".tif"))
    } else {
      ans[[i]] <- cropped
    }
  }
  if (!write) {
    attr(ans, "class") <- "rasterBlocks"
    attr(ans, "package") <- "enerscape"
    return(ans)
  }
}

#' @title Merge split raster blocks
#'
#' @param x an enerscape rasterBlocks class, which is a list of raster blocks.
#' @param out.dir string, output directory; default to tempdir().
#' @param pattern string, regular pattern to subset files in out.dir.
#'
#' @details if x == NULL, rasterMerge() loads the rasters save to disk by
#' rasterSplit()
#'
#' @return a RasterLayer.
#'
#' @examples
#' \donttest{
#' library(raster)
#' data("volcano")
#' r <- raster(volcano)
#' crs(r) <- "+proj=longlat +datum=WGS84 +no_defs"
#' gr <- gridRaster(r, split.hor = 3, split.vert = 3)
#' splits <- splitRaster(r, gr)
#' }
mergeRaster <- function(
  x = NULL,
  out.dir = NULL,
  pattern = ""
) {
  if (is.null(x) & is.null(out.dir)) stop("Either of x or out.dir must be specified.")
  if (!is.null(x) & !is.null(out.dir)) stop("Only one of x or out.dir must be specified.")
  if (is.null(x)) {
    files <- list.files(out.dir, pattern = ".tif$", full.names = TRUE)
    files <- files[grepl(pattern, files)]
    if (length(files) == 0) {
      stop("No RasterLayer found with pattern: ", pattern)
    }
    x <- list()
    for (f in files) x[[which(files == f)]] <- raster::raster(f)
  }
  for (lyr in x) {
    if (identical(lyr, x[[1]])) {
      merged <- lyr
    } else {
      merged <- raster::merge(merged, lyr)
    }
  }
  return(merged)
}

#' @title Runs enerscape on blocks of a RasterLayer.
#'
#' @inherit enerscape
#' @param split.hor integer, number of horizontal splits.
#' @param split.vert integer, number of vertical splits.
#'   avoid missing values at the polygon border when merging back together.
#' @param write TRUE/FALSE, if to write the raster to disk.
#' @param out.dir string, output directory; default to tempdir().
#'
#' @details Split the DEM into blocks and run enerscape for each block
#'  sequentially.
#' This is useful when the DEM layer is particularly large. There are two
#' options: the first is to manipulate all layers in memory (write = FALSE);
#' the second is to write each layer to disk and load it when necessary.
#' The former is faster, but consume more RAM.
#'
#' @examples
#' \donttest{
#' library(raster)
#' library(enerscape)
#' data("volcano")
#' r <- raster(volcano)
#' crs(r) <- "+proj=longlat +datum=WGS84 +no_defs"
#' r <- projectRaster(r, crs = "EPSG:32759", res = c(1e3, 1e3))
#' en <- enerscape(r, 1)$rasters$EnergyScape
#' en_gr <- enerscape_gridded(r, 1, split.hor = 3, split.vert = 3)
#' }
enerscape_gridded <- function(
  dem,
  m,
  split.hor,
  split.vert,
  unit = "joule",
  neigh = 16,
  method = "ARC",
  v = NULL,
  write = FALSE,
  out.dir = raster::tmpDir()
) {
  # make spatial grid
  gr <- gridRaster(dem, split.hor, split.vert)
  if (write) message("Single enerscape grids are written in ", out.dir)
  # make raster blocks
  if (write) {
    blocks <- splitRaster(dem, gr, write = TRUE, out.dir = out.dir)
    files <- list.files(out.dir, pattern = ".tif$", full.names = TRUE)
    files <- files[grepl("grid-", files)]
    files <- files[grepl("tif", files)]
    blocks <- list()
    for (f in files) blocks[[which(files == f)]] <- raster::raster(f)
  } else {
    blocks <- splitRaster(dem, gr)
  }
  # run enerscape for each block
  i <- 1
  if (!write) ans <- list()
  for (lyr in blocks) {
    message(" - Running enerscape for grid ", i, " of ", length(blocks))
    en <- suppressMessages(enerscape(lyr, m, unit, neigh, method, v))
    en <- en$rasters$EnergyScape
    en <- raster::crop(en, gr[i, ]) #crop to remove buffers
    if (write) {
      raster::writeRaster(en, paste0(out.dir, "enerscape-", i, ".tif"))
    } else {
      ans[[i]] <- en
    }
    i <- i + 1
  }
  if (write) {
    en <- mergeRaster(out.dir = out.dir, pattern = "enerscape-")
  } else {
    en <- mergeRaster(ans)
  }
  return(en)
}
