% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{energyscape}
\alias{energyscape}
\title{Energy Landscape}
\usage{
energyscape(
  x,
  n = 4L,
  mass = 0,
  res = 0,
  kcal = TRUE,
  out = 0L,
  direction = 0L
)
}
\arguments{
\item{x}{matrix with values the elevation.}

\item{n}{(integer) number of neighbours to consider (either 4 or 8).}

\item{mass}{body mass of species (kg).}

\item{res}{numeric value (double) of the spatial resolution of the matrix.}

\item{kcal}{(boolean) if to return the result in kCal (true) or J (false).}

\item{out}{(integer) if to calculate the costs for moving into the cell (0) or from it (1).}

\item{direction}{(integer) for which direction to calculate costs: 0 for all, 1 for left, 2 for down, 3 for right, 4 for up.}
}
\value{
Matrix with the energy cost of locomotion (EnergyScape).
}
\description{
Energy Landscape
}
