#' Create the initialization file for the julia package Circuitscape
#'
#' This creates the init file for the julia package Circuitscape:
#' \url{https://juliapackages.com/p/circuitscape}.
#' @param en an enerscape object.
#' @param path full path where to write the .ini file.
#' @param points data.frame with origin and destination coordinates.
#' @return Nothing, only write the circuitscape.ini file to disk.
#' @export
circuitscape_skeleton <- function(
  en = NULL,
  path = NULL,
  points = NULL
) {
  if (is.null(en) | is.null(points)) {
    stop("Missing mandatory input")
  }
  if (is(points, "data.frame")) {
      points <- as.matrix(points)
    } else {
      stop("points are not data.frame or matrix")
    }
  if (is.null(path)) path <- getwd()
  points <- vect(points,
                 crs = crs(en),
                 atts = data.frame(ID = seq_len(nrow(points))))
  # points <- buffer(points, res(en)[1] * 3)
  pr <- rasterize(points, en, na.rm = TRUE, field = "ID")
  writeRaster(en,
              file.path(path, "EnergyScape.tif"),
              overwrite = TRUE)
  writeRaster(pr,
              file.path(path, "loc.tif"),
              overwrite = TRUE)
  cs_file <- file(file.path(path, "circuitscape.ini"), open = "w")
  if (!isOpen(cs_file)) {
    stop("Connection to file cannot be established")
  }
  writeLines(text = c(
    "[Circuitscape Mode]",
    "data_type = raster",
    "scenario = pairwise",
    "",
    "[Version]",
    "version = 5.0.0",
    "",
    "[Habitat raster or graph]",
    paste0("habitat_file = ", path, "/EnergyScape.tif"),
    "habitat_map_is_resistances = true",
    "",
    "[Connection Scheme for raster habitat data]",
    "connect_four_neighbors_only = false",
    "connect_using_avg_resistances = false",
    "",
    "[Short circuit regions (aka polygons)]",
    "use_polygons = true",
    paste0("polygon_file = ", path, "/loc.tif"),
    "",
    "[Options for pairwise and one-to-all and all-to-one modes]",
    paste0("point_file = ", path, "/loc.tif"),
    "",
    "[Calculation options]",
    "solver = cg+amg",
    "",
    "[Output options]",
    "write_cum_cur_map_only = false",
    "log_transform_maps = false",
    paste0("output_file = ", path, "/circuitscape"),
    "write_max_cur_maps = false",
    "write_volt_maps = false",
    "set_null_currents_to_nodata = true",
    "set_null_voltages_to_nodata = true",
    "compress_grids = false",
    "write_cur_maps = true"
  ),
  con = cs_file)
  close(cs_file)
}
