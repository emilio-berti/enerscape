#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector slope ( NumericVector dem , double center , double res ) {
  const double PI =  3.1415926535;
  int n = dem.size();
  NumericVector ans(n);
  double h;
  for (int i = 0; i < n; i++) {
    h = center - dem[i];
    ans[i] = atan(h / res) * 180 / PI;
  }
  return ans;
}

// [[Rcpp::export]]
NumericVector distance ( NumericVector dem , double center , double res ) {
  const double PI =  3.1415926535;
  int n = dem.size();
  NumericVector ans(n);
  double h;
  for (int i = 0; i < n; i++) {
    h = center - dem[i];
    ans[i] = sqrt(pow(res, 2) + pow(h, 2));
  }
  return ans;
}
