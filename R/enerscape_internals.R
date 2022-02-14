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
.calc_arc <- function(
  slope,
  m,
  work_in_kcal = TRUE,
  j_to_kcal = 4184
) {
  en_res <- get("en_res", envir = parent.frame())
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
#' @inherit .calc_arc
#' @return A transition layer with values the conductance between cells, i.e.
#'   the distance that can be traveled per unit of energy (1 / J or 1 / kcal).
#' @export
.calc_arc_cond <- function(
  slope,
  m,
  work_in_kcal = TRUE,
  j_to_kcal = 4184
) {
  en_res <- get("en_res", envir = parent.frame())
  E_ar <- 8 * m ^ (-0.34)
  E_mec <- 100 * (1 + sin((2 * slope - 74) / 180 * pi)) * m ^ (-0.12)
  work <- (E_ar + E_mec) * m * en_res / abs(cos(slope * pi / 180))
  if (work_in_kcal) {
    work <- work / j_to_kcal
  }
  cond <- 1 / work
  return(cond)
}
#' Compute energy costs for a cyclist
#'
#' Internal function for enerscape - calculate work.
#' @param height height transition matrix.
#' @param slope slope transition matrix.
#' @param m species body mass (kg).
#' @param v speed of cyclist.
#' @param work_in_kcal if work should be expressed in kilocalories.
#' @param j_to_kcal joules to kilocalories conversion constant.
#' @return A transition layer with values the energy cost of movement between
#'   cells (J or kcal).
#' @details Internal function of enerscape, don't call directly. This assumes no
#'   wind, a bike of 7 kg, optimal pedal frequency, and constant mechanical
#'   efficiency of 25%.
.calc_cycling <- function(
  height,
  slope,
  m,
  v,
  work_in_kcal = TRUE,
  j_to_kcal = 4184
) {
  en_res <- get("en_res", envir = parent.frame())
  m <- m + 7 #add mass of bike
  temp <- (15.04 - 0.00649 * height) #air temperature
  atm <- 101.29 * ((temp + 273.1) / 288.08) ^ 5.256 #atmospheric pressure
  atm <- atm * 7.500638 #in Torr
  temp <- temp + 272.15 #in Kelvin
  VO2 <- (8.6 * 10^-3 * m +
            7.8 * 10^-3 * 1.8 * atm / temp * v^2 +
            1.87 * m * sin(slope / 180 * pi)) * v
  E <- VO2 * 20.9 #ml O2 to J
  E <- E * en_res / abs(cos(slope * pi / 180)) / v
  if (work_in_kcal) {
    E <- E / j_to_kcal
  }
  return(E)
}
#' Compute conductance for a cyclist
#'
#' Internal function for enerscape - calculate work.
#' @inherit .calc_cycling
.calc_cycling_cond <- function(
  height,
  slope,
  m,
  v,
  work_in_kcal = TRUE,
  j_to_kcal = 4184
) {
  en_res <- get("en_res", envir = parent.frame())
  m <- m + 7 #add mass of bike
  temp <- (15.04 - 0.00649 * height) #air temperature
  atm <- 101.29 * ((temp + 273.1) / 288.08) ^ 5.256 #atmospheric pressure
  atm <- atm * 7.500638 #in Torr
  temp <- temp + 272.15 #in Kelvin
  VO2 <- (8.6 * 10^-3 * m +
            7.8 * 10^-3 * 1.8 * atm / temp * v^2 +
            1.87 * m * sin(slope / 180 * pi)) * v
  E <- VO2 * 20.9 #ml O2 to J
  E <- E * en_res / abs(cos(slope * pi / 180)) / v
  if (work_in_kcal) {
    E <- E / j_to_kcal
  }
  return(1 / E)
}
