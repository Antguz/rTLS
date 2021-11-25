#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericVector euclidean_rcpp(Rcpp::NumericVector sample, Rcpp::NumericMatrix base, int threads = 1) {

  NumericVector distance(base.nrow());

#ifdef _OPENMP
  if ( threads > 0 ) {
    omp_set_num_threads( threads );
  }
#endif

#pragma omp parallel for
  for (int i = 0; i < base.nrow(); i++) {

    distance[i] = sqrt((pow(base(i, 0) - sample[0], 2.0) + pow(base(i, 1) - sample[1], 2.0) + pow(base(i, 2) - sample[2], 2.0)));

  }

  return distance;
}
