#ifndef EUCLIDEAN_DIST_H
#define EUCLIDEAN_DIST_H

#include <Rcpp.h>

Rcpp::NumericVector euclidean_rcpp(Rcpp::NumericVector sample, Rcpp::NumericMatrix base, int threads = 1);

#endif
