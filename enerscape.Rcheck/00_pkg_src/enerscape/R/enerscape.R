#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#' @inherit slope_to_dem
#' @inherit calc_work

enerscape <- function(
  dem,
  m,
  output_to_disk = FALSE,
  output_file = NULL,
  units = "J"
) {
  if (output_to_disk & is.null(output_file)) {
    stop("Specify a destination directory for the output.")
  }
  message("Calculating slope")
  slope <- dem_to_slope(dem, output_to_disk, output_file)
  message("Calculating work")
  if (units == "J"){
    work <- calc_work(slope, m, output_to_disk, output_file)
  } else if (units == "kcal") {
    work <- calc_work(slope, m, output_to_disk, output_file, work_in_kcal = TRUE)
  } else {
    stop("Units not J or kcal.")
  }
  if (class(dem) == "character") {
    dem <- raster::raster(dem)
    ans <- raster::stack(c(dem, slope, work))
  } else if (class(dem) == "RasterLayer") {
    ans <- raster::stack(c(dem, slope, work))
  } else {
    # slope <- terra::rast(slope)
    # work <- terra::rast(work)
    ans <- c(dem, slope, work)
  }
  names(ans) <- c("dem", "slope", "work")
  return(ans)
}
