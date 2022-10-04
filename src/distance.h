#include <Rcpp.h>
using namespace Rcpp;

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
  //diagonal corrections
  if (n == 8) {
    h = center - dem[0];
    ans[0] = sqrt(2 * pow(res, 2) + pow(h, 2));
    h = center - dem[2];
    ans[2] = sqrt(2 * pow(res, 2) + pow(h, 2));
    h = center - dem[5];
    ans[5] = sqrt(2 * pow(res, 2) + pow(h, 2));
    h = center - dem[7];
    ans[7] = sqrt(2 * pow(res, 2) + pow(h, 2));
  }
  return ans;
}
