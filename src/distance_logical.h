#ifndef DISTANCELOGICAL_H
#define DISTANCELOGICAL_H

#include <RcppArmadillo.h>

Rcpp::NumericVector distance_logical(const Rcpp::NumericVector sample, const Rcpp::NumericMatrix base, const Rcpp::NumericVector radius);

#endif
