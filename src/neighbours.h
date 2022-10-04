#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector neighbours ( int i , int j , int n , NumericMatrix m ) {
  NumericVector ans(n);
  int rows = m.nrow();
  int cols = m.ncol();
  if (j == (cols - 1) || j == 0) {
    return NULL;
  }
  if (i == (rows - 1) || i == 0) {
    return NULL;
  }
  if (n == 4) {
    ans[0] = m(i - 1, j);
    ans[1] = m(i, j - 1);
    ans[2] = m(i, j + 1);
    ans[3] = m(i + 1, j);
    return ans;
  } else if (n == 8) {
    ans[0] = m(i - 1, j - 1);
    ans[1] = m(i, j - 1);
    ans[2] = m(i + 1, j - 1);
    ans[3] = m(i - 1, j);
    ans[4] = m(i + 1, j);
    ans[5] = m(i - 1, j + 1);
    ans[6] = m(i, j + 1);
    ans[7] = m(i + 1, j + 1);
    return ans;
  } else {
    std::cout << "Number of neighoours must be 4 or 8.";
    return NULL;
  }
}
