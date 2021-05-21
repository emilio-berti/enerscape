#' Create the initialization file for the julia package Omniscape
#'
#' This creates the init file for the julia package Omniscape:
#' \url{https://juliapackages.com/p/omniscape}.
#' @param en an enerscape object.
#' @param path full path where to write the .ini file.
#' @param radius radius in pixels of the moving window.
#' @param aggr_fact the block size to compute the Omniscape.
#' @return Nothing, only write the omniscape.ini file to disk.
#' @export
omniscape_skeleton <- function(
  en = NULL,
  path = NULL,
  radius = NULL,
  aggr_fact = 1
) {
  if (is.null(en) | is.null(path) | is.null(radius)) {
    stop("Missing mandatory input")
  }
  if (aggr_fact > 1) {
    w <- raster::aggregate(en$rasters$EnergyScape, aggr_fact)
  } else {
    w <- en$rasters$EnergyScape
  }
  raster::writeRaster(w, file.path(path, "/work.tif"),
                      overwrite = TRUE)
  omni_file <- file(file.path(path, "omniscape.ini"), open = "w")
  if (!isOpen(omni_file)) {
    stop("Connection to file cannot be established")
  }
  writeLines(text = c(
    "[Required arguments]",
    paste0("resistance_file = ", path, "/work.tif"),
    paste0("radius = ", radius),
    paste0("block_size = ", 1),
    paste0("project_name = ", path, "/omniscape"),
    "",
    "[General options]",
    "source_from_resistance = true",
    "calc_normalized_current = true",
    "",
    "parallelize = true",
    "parallel_batch_size = 10",
    "",
    "[Output options]",
    "write_raw_currmap = true"),
    con = omni_file)
  close(omni_file)
}
