#ifndef FEATURES_RADIUS_H
#define FEATURES_RADIUS_H

#include <RcppArmadillo.h>

arma::cube features_radius_rcpp(arma::mat index, arma::mat query, arma::vec radius, int threads = 1, bool progress = true);

#endif
