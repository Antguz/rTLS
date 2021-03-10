#ifndef SPHERE_COVERING_H
#define SPHERE_COVERING_H

#include <RcppArmadillo.h>

arma::mat sphere_covering_rcpp(arma::mat query, int k , std::string build, int threads, int checks);

#endif
