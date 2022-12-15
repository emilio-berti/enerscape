#include <Rcpp.h>
using namespace Rcpp;

//' Neighbours
//'
//' @param i row index
//' @param j column index
//' @param n number of neighbours (4 or 8)
//' @param x matrix with values
//' @return Vector with values the neighours of x
// [[Rcpp::export]]
NumericVector neighbours ( int i , int j , int n , NumericMatrix x ) {
  NumericVector ans(n);
  int rows = x.nrow();
  int cols = x.ncol();
  if (j == (cols - 1) || j == 0) {
    return NULL;
  }
  if (i == (rows - 1) || i == 0) {
    return NULL;
  }
  if (n == 4) {
    ans[0] = x(i - 1, j);
    ans[1] = x(i, j - 1);
    ans[2] = x(i, j + 1);
    ans[3] = x(i + 1, j);
    return ans;
  } else if (n == 8) {
    ans[0] = x(i - 1, j - 1); //diag
    ans[1] = x(i, j - 1);
    ans[2] = x(i + 1, j - 1); //diag
    ans[3] = x(i - 1, j);
    ans[4] = x(i + 1, j);
    ans[5] = x(i - 1, j + 1); //diag
    ans[6] = x(i, j + 1);
    ans[7] = x(i + 1, j + 1); //diag
    return ans;
  } else {
    return NULL;
  }
}
