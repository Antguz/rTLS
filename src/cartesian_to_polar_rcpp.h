#ifndef CARTESIAN_TO_POLAR_H
#define CARTESIAN_TO_POLAR_H

#include <Rcpp.h>

NumericMatrix cartesian_to_polar_rcpp(NumericMatrix cartesian, NumericVector anchor, int threads = 1);

#endif
