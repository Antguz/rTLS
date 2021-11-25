#ifndef CIRCLERASAC
#define CIRCLERASAC

#include <RcppArmadillo.h>

arma::mat circleRANSAC_rcpp(arma::mat cloud, double fpoints, double z_value, arma::vec poutlier, int max_iterations, int threads = 1);

#endif
