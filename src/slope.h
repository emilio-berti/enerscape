#ifndef SLOPE_H
#define SLOPE_H

#include <Rcpp.h>
using namespace Rcpp;

NumericVector slope ( NumericVector x , double center , double res );
NumericVector slopeRadiant ( NumericVector x , double center , double res );

#endif
