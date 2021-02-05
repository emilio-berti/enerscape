#' Check if model extrapolates
#'
#' This check if computation of the energy landscape extrapolates from the test
#' set of enerscape::pontzer (2016).
#' @param en an enerscape object.
#' @param plot plot areas where slope is extrapolated.
#' @return A list with booleans if body size or inclines extrapolates and a
#'   rasterLayer for where incline extrapolates. The rasterLayer is returned
#'   only if extrapolations are present.
#' @details Check if body mass or incline are outside the test range of the
#'   model. If slope extrapolations are detected and \code{plot = TRUE}, a plot
#'   of where extrapolations occur is displayed.
#' @examples
#' library(raster)
#' library(enerscape)
#' data("volcano")
#' dem <- raster(volcano)
#' en <- enerscape(dem, 10, unit = "kcal", neigh = 16)
#' en_extrapolation(en, plot = TRUE)
#' @export
#' @references enerscape::pontzer, H. (2016). A unified theory for the energy
#'   cost of legged locomotion. Biology Letters, 12(2), 20150935. \doi{
#'   https://doi.org/10.1098/rsbl.2015.0935}.
en_extrapolation <- function(en,
                             plot = TRUE) {
  extr_mass <- ifelse(en$mass >= min(enerscape::pontzer$Mass) &
                         en$mass <= max(enerscape::pontzer$Mass),
                       FALSE,
                       TRUE)
  v <- raster::values(en$rasters$Slope)
  min_slope <- min(v, na.rm = TRUE)
  max_slope <- max(v, na.rm = TRUE)
  extr_slope <- ifelse(min_slope >= min(enerscape::pontzer$Incline) &
                         max_slope <= max(enerscape::pontzer$Incline),
    FALSE,
    TRUE)
  extr_rast <- en$rasters$Slope
  if (extr_slope) {
    extr_rast[extr_rast >= min(enerscape::pontzer$Incline) &
                extr_rast <= max(enerscape::pontzer$Incline)] <- NA
    extr_rast[!is.na(extr_rast)] <- 1
  } else {
    extr_rast <- NULL
  }
  if (plot & extr_slope) {
    raster::plot(en$rasters$Slope)
    raster::plot(extr_rast,
                 add = TRUE,
                 legend = FALSE,
                 col = grDevices::adjustcolor("blue", alpha.f = 0.5))
  } else if (plot & !extr_slope) {
    message("No slope extrapolation detected")
  }
  return(list("Mass extrapolated" = extr_mass,
              "Slope extrapolated" = extr_slope,
              "Slope extrapolation" = extr_rast))
}
