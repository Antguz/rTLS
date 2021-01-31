#ifndef MINIMUM_DISTANCE_H
#define MINIMUM_DISTANCE_H

#include <Rcpp.h>

double minimum_distance_rcpp(arma::mat amat, double radius, int threads = 1, bool progress = true);

#endif
