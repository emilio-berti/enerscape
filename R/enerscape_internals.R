#' Compute slope
#'
#' Internal function for enerscape - calculate slope
#' @param x a digital elevation model raster layer
#' @return A transition layer with values the inclines between cells (degrees).
#' @details Internal function of enerscape, don't call directly.
.calc_slope <- function(x) {
  diff <- x[2] - x[1]
  ipo <- sqrt(en_res ^ 2 + diff ^ 2)
  alpha <- asin(diff / ipo) * 180 / pi
  if (is.na(alpha)) {
    alpha <- 0
  }
  return(alpha)
}
#' Compute energy costs
#'
#' Internal function for enerscape - calculate work.
#' @param slope slope transition matrix.
#' @param m species body mass (kg).
#' @param work_in_kcal if work should be expressed in kilocalories.
#' @param j_to_kcal joules to kilocalories conversion constant.
#' @return A transition layer with values the energy cost of movement between
#'   cells (J or kcal).
#' @details Internal function of enerscape, don't call directly.
.calc_work <- function(
  slope,
  m,
  work_in_kcal = TRUE,
  j_to_kcal = 4184
) {
  E_ar <- 8 * m ^ (-0.34)
  E_mec <- 100 * (1 + sin((2 * slope - 74) / 180 * pi)) * m ^ (-0.12)
  work <- (E_ar + E_mec) * m * en_res / abs(cos(slope * pi / 180))
  if (work_in_kcal) {
    work <- work / j_to_kcal
  }
  return(work)
}
#' Compute conductance
#'
#' Internal function for enerscape - calculate conductance
#' @inherit .calc_work
#' @return A transition layer with values the conductance between cells, i.e.
#'   the distance that can be travelled per unit of energy (1 / J or 1 / kcal).
#' @export
.calc_cond <- function(
  slope,
  m,
  work_in_kcal = TRUE,
  j_to_kcal = 4184
) {
  E_ar <- 8 * m ^ (-0.34)
  E_mec <- 100 * (1 + sin((2 * slope - 74) / 180 * pi)) * m ^ (-0.12)
  work <- (E_ar + E_mec) * m * en_res / abs(cos(slope * pi / 180))
  if (work_in_kcal) {
    work <- work / j_to_kcal
  }
  cond <- 1 / work
  return(cond)
}
