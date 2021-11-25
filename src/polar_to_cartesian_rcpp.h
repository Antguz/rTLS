#ifndef POLAR_TO_CARTESIAN_H
#define POLAR_TO_CARTESIAN_H

#include <Rcpp.h>

NumericMatrix polar_to_cartesian_rcpp(NumericMatrix polar, int threads = 1);

#endif
