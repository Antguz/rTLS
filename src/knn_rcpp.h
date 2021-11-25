#ifndef KNN_H
#define KNN_H

#include <RcppArmadillo.h>

arma::mat knn_rcpp(arma::mat query, arma::mat ref, int k, bool same, std::string build, int threads, int checks);

#endif
