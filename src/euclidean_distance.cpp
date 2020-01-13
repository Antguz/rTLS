// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericVector euclidean_distance(Rcpp::NumericVector sample, Rcpp::NumericMatrix base) {

  NumericVector distance(base.nrow());

  for (int i = 0; i < base.nrow(); i++) {

    distance[i] = sqrt((pow(base(i, 0) - sample[0], 2.0) + pow(base(i, 1) - sample[1], 2.0) + pow(base(i, 2) - sample[2], 2.0)));

  }

  return distance;
}
