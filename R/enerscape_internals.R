#' Internal function for enerscape - calculate slope
#' @param x a digital elevation model raster layer
.calc_slope <- function(x) {
  diff <- x[2] - x[1]
  ipo <- sqrt(en_res ^ 2 + diff ^ 2)
  alpha <- asin(diff / ipo) * 180 / pi
  if (is.na(alpha)) {
    alpha <- 0
  }
  return(alpha)
}
#' Internal function for enerscape - calculate work.
#' @inherit enerscape
#' @param slope slope transition matrix.
#' @param work_in_kcal if work should be expressed in kilocalories.
#' @param j_to_kcal joules to kilocalories conversion constant.
.calc_work <- function(
  slope,
  m,
  work_in_kcal = TRUE,
  j_to_kcal = 4184
) {
  E_ar <- 8 * m ^ (-0.34)
  E_mec <- 100 * (1 + sin((2 * slope - 74) / 180 * pi)) * m ^ (-0.12)
  work <- (E_ar + E_mec) * m * en_res / cos(slope * pi / 180)
  if (work_in_kcal) {
    work <- work / j_to_kcal
  }
  return(work)
}
#' Internal function for enerscape - calculate conductance
#' @inherit .calc_work
.calc_cond <- function(
  slope,
  m,
  work_in_kcal = TRUE,
  j_to_kcal = 4184
) {
  E_ar <- 8 * m ^ (-0.34)
  E_mec <- 100 * (1 + sin((2 * slope - 74) / 180 * pi)) * m ^ (-0.12)
  work <- (E_ar + E_mec) * m * en_res / cos(slope * pi / 180)
  if (work_in_kcal) {
    work <- work / j_to_kcal
  }
  cond <- 1 / work
  return(cond)
}
