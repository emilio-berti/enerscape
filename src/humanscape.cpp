#include <Rcpp.h>
#include "neighbours.h"
#include "slope.h"
#include "distance.h"
using namespace Rcpp;

//' Energy Landscape for walking people
//'
//' @param v speed
//' @param slope vector with slopes
//' @param distance vector with distances
//' @param mass body mass of species (kg)
//' @param res numeric value (double) of the spatial resolution of the matrix
//' @param kcal (boolean) if to return the result in kCal (true) or J (false)
//' @return Vector with the energy cost of locomotion (EnergyScape)
// [[Rcpp::export]]
NumericVector energyHuman (
  double mass,
  double v,
  NumericVector slope,
  NumericVector distance,
  double res,
  bool kcal
) {
  const double KCAL = 4184.0;
  int n = slope.size();
  double flat;
  NumericVector hill(n);
  NumericVector work(n);
  flat = 1.44 + 1.94 * pow(v, 0.34) + 0.24 * pow(v, 4);
  for (int i = 0; i < n; i++) {
    hill[i] = 34 * v * tan(slope[i]) * ( 1 - pow( 1.05 , 1 - pow( 1.1 , 100 * tan(slope[i]) + 32 ) ) );
    work[i] = (flat + hill[i]) / v * mass * distance[i];
    if (kcal) {
      work[i] = work[i] / KCAL;
    }
  }
  return work;
}

//' Energy Landscape
//'
//' @param x matrix with values the elevation.
//' @param v speed
//' @param n (integer) number of neighbours to consider (either 4 or 8).
//' @param mass body mass of species (kg).
//' @param res numeric value (double) of the spatial resolution of the matrix.
//' @param kcal (boolean) if to return the result in kCal (true) or J (false).
//' @return Matrix with the energy cost of locomotion (EnergyScape).
// [[Rcpp::export]]
NumericMatrix energyscapeHuman (
    NumericMatrix x,
    double mass,
    double v,
    int n,
    double res,
    bool kcal
) {
  if (mass == 0 || res == 0) {
    return 0;
  }
  int rows = x.nrow();
  int cols = x.ncol();
  NumericVector neigh(n);
  NumericVector sl(n);
  NumericVector dist(n);
  NumericVector en(n);
  NumericMatrix ans(rows, cols);
  for (int i = 0 ; i < rows ; i++) {
    for (int j = 0 ; j < cols ; j++) {
      neigh = neighbours(i, j, n, x);
      if (neigh.size() == 0) {
        continue;
      }
      sl = slopeRadiant(neigh, x(i, j), res);
      dist = distances(neigh, x(i, j), res);
      en = energyHuman(mass, v, sl, dist, res, kcal);
      ans(i, j) = mean(en);
    }
  }
  return ans;
}
