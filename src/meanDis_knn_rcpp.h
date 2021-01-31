#ifndef MEANDIS_KNN_H
#define MEANDIS_KNN_H

#include <RcppArmadillo.h>

arma::vec meanDis_knn_rcpp(arma::mat amat, int k, int threads = 1, bool progress = true);

#endif
