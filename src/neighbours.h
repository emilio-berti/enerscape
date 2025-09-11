#ifndef NEIGHBOURS_H
#define NEIGHBOURS_H

#include <Rcpp.h>
using namespace Rcpp;

NumericVector neighbours ( int i , int j , int n , NumericMatrix x );

#endif
