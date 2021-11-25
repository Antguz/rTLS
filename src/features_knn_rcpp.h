#ifndef FEATURES_KNN_H
#define FEATURES_KNN_H

#include <RcppArmadillo.h>

arma::cube features_knn_rcpp(arma::mat index, arma::mat query, arma::vec k, int threads = 1, bool progress = true);

#endif
