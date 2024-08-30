#include <Rcpp.h>
using namespace Rcpp;

//' Slopes
//'
//' @param x matrix with values
//' @param center numeric value (double) with the value of the focal cell
//' @param res numeric value (double) of the spatial resolution of the matrix
//' @param out (integer) if to calculate the costs for moving into the cell (0) or from it (1).
//' @return Vector with values the slopes (degrees) between x and center
// [[Rcpp::export]]
NumericVector slope (
  NumericVector x,
  double center,
  double res,
  int out
) {
  const double PI =  3.1415926535;
  int n = x.size();
  NumericVector ans(n);
  double h;
  for (int i = 0; i < n; i++) {
    h = center - x[i];
    ans[i] = atan(h / res) * 180 / PI;
  }
  //diagonal corrections
  if (n == 8) {
    h = center - x[0];
    ans[0] = atan(h / (sqrt(2) * res)) * 180 / PI;
    h = center - x[2];
    ans[2] = atan(h / (sqrt(2) * res)) * 180 / PI;
    h = center - x[5];
    ans[5] = atan(h / (sqrt(2) * res)) * 180 / PI;
    h = center - x[7];
    ans[7] = atan(h / (sqrt(2) * res)) * 180 / PI;
  }
  if (out == 1) {
    ans = -ans;
  }
  return ans;
}
