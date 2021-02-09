#ifndef RAY_AABB_H
#define RAY_AABB_H

#include <RcppArmadillo.h>

arma::vec line_AABB_rcpp(arma::mat orig, arma::mat end, arma::vec AABB_min, arma::vec AABB_max);

#endif
