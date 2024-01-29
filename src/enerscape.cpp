#include <Rcpp.h>
#include "neighbours.h"
#include "slope.h"
#include "distance.h"
using namespace Rcpp;

//' Energy Landscape
//'
//' @param slope vector with slopes
//' @param distance vector with distances
//' @param mass body mass of species (kg)
//' @param res numeric value (double) of the spatial resolution of the matrix
//' @param kcal (boolean) if to return the result in kCal (true) or J (false)
//' @return Vector with the energy cost of locomotion (EnergyScape)
// [[Rcpp::export]]
NumericVector energy (
    NumericVector slope ,
    NumericVector distance ,
    double mass ,
    double res ,
    bool kcal = true
) {
  const double PI =  3.1415926535;
  const double KCAL = 4184.0;
  int n = slope.size();
  NumericVector ar(n);
  NumericVector mec(n);
  NumericVector work(n);
  for (int i = 0; i < n; i++) {
    ar[i] = 8.0 * pow(mass, -0.34);
    mec[i] = 50.0 * (1.0 + sin((2.0 * slope[i] - 74.0) / 180.0 * PI)) * pow(mass, -0.12);
    work[i] = (ar[i] + mec[i]) * mass * distance[i];
    if (kcal) {
      work[i] = work[i] / KCAL;
    }
  }
  return work;
}

//' Energy Landscape
//'
//' @param x matrix with values the elevation.
//' @param n (integer) number of neighbours to consider (either 4 or 8).
//' @param mass body mass of species (kg).
//' @param res numeric value (double) of the spatial resolution of the matrix.
//' @param kcal (boolean) if to return the result in kCal (true) or J (false).
//' @return Matrix with the energy cost of locomotion (EnergyScape).
// [[Rcpp::export]]
NumericMatrix energyscape (
    NumericMatrix x,
    int n = 4,
    double mass = 0,
    double res = 0,
    bool kcal = true
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
      sl = slope(neigh, x(i, j), res);
      dist = distances(neigh, x(i, j), res);
      en = energy(sl, dist, mass, res, kcal);
      ans(i, j) = mean(en);
    }
  }
  return ans;
}
