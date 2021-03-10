#ifndef LINES_INTERCEPTION_H
#define LINES_INTERCEPTION_H

#include <RcppArmadillo.h>

arma::mat lines_interception_rcpp(arma::mat orig, arma::mat dir, arma::mat voxels, arma::vec edge_length, int threads = 1, bool progress = true);

#endif
