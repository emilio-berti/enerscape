#include <Rcpp.h>
using namespace Rcpp;

//' Spatial distances
//'
//' @param x matrix with values
//' @param center numeric value (double) with the value of the focal cell
//' @param res numeric value (double) of the spatial resolution of the matrix
//' @return Vector with values the distances between x and center
// [[Rcpp::export]]
NumericVector distances ( NumericVector x , double center , double res ) {
  int n = x.size();
  NumericVector ans(n);
  double h;
  for (int i = 0; i < n; i++) {
    h = center - x[i];
    ans[i] = sqrt(pow(res, 2) + pow(h, 2));
  }
  //diagonal corrections
  if (n == 8) {
    h = center - x[0];
    ans[0] = sqrt(2 * pow(res, 2) + pow(h, 2));
    h = center - x[2];
    ans[2] = sqrt(2 * pow(res, 2) + pow(h, 2));
    h = center - x[5];
    ans[5] = sqrt(2 * pow(res, 2) + pow(h, 2));
    h = center - x[7];
    ans[7] = sqrt(2 * pow(res, 2) + pow(h, 2));
  }
  return ans;
}
