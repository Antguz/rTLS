#ifndef GETEIGENVALUES_H
#define GETEIGENVALUES_H

#include <RcppArmadillo.h>

Rcpp::NumericVector getEigenvalues(Rcpp::NumericMatrix base, Rcpp::NumericVector dis_logical);

#endif
