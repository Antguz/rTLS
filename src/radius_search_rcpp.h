#ifndef RADIUS_SEARCH_H
#define RADIUS_SEARCH_H

#include <RcppArmadillo.h>

arma::mat radius_search_rcpp(arma::mat query, arma::mat ref, double radius, int max_neighbour, bool same, std::string build, int threads, int checks);

#endif
