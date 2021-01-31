#ifndef DIMENSIONALITY_KNN_H
#define DIMENSIONALITY_KNN_H

#include <RcppArmadillo.h>

arma::cube dimensionality_knn_rcpp(arma::mat amat, arma::mat bmat, arma::vec k, int threads = 1, bool progress = true);

#endif
