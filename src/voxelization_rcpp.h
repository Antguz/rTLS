#ifndef VOXELIZATION_H
#define VOXELIZATION_H

#include <RcppArmadillo.h>

arma::mat voxelization_rcpp(arma::mat cloud, arma::vec edge_length, int threads = 1);

#endif
