#include <Rcpp.h>
using namespace Rcpp;

//' Slopes
//'
//' @param x matrix with values
//' @param center numeric value (double) with the value of the focal cell
//' @param res numeric value (double) of the spatial resolution of the matrix
//' @return Vector with values the slopes (degrees) between x and center
// [[Rcpp::export]]
NumericVector slope ( NumericVector x , double center , double res ) {
  const double PI =  3.1415926535;
  int n = x.size();
  NumericVector ans(n);
  double h;
  for (int i = 0; i < n; i++) {
    h = x[i] - center;
    ans[i] = atan(h / res) * 180 / PI;
  }
  //diagonal corrections
  if (n == 8) {
    h = x[0] - center;
    ans[0] = atan(h / (sqrt(2) * res)) * 180 / PI;
    h = x[2] - center;
    ans[2] = atan(h / (sqrt(2) * res)) * 180 / PI;
    h = x[5] - center;
    ans[5] = atan(h / (sqrt(2) * res)) * 180 / PI;
    h = x[7] - center;
    ans[7] = atan(h / (sqrt(2) * res)) * 180 / PI;
  }
  return ans;
}
