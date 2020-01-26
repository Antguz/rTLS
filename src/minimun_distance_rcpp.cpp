#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppArmadillo"]]
// [[Rcpp::depends(RcppProgress)]]
#include <RcppArmadillo.h>
#include <progress.hpp>
#include <progress_bar.hpp>

using arma::sqrt;
using arma::pow;
using arma::min;
using namespace arma;

// [[Rcpp::export]]
double minimun_distance_rcpp(arma::mat amat, int threads = 1, bool progress = true) {

#ifdef _OPENMP
  if ( threads > 0 ) {
    omp_set_num_threads( threads );
    REprintf("Number of threads=%i\n", omp_get_max_threads());
  }
#endif

  int an = amat.n_rows;

  arma::vec out(an);

  Progress p(an, progress);

#pragma omp parallel for
  for (int i = 0; i < an; i++) {
    arma::vec distance(an);

    for (int j = 0; j < an; j++) { //Loop to estimate the distance
      distance(j) = sqrt(pow((amat(j, 0) - amat(i, 0)), 2.0) + pow((amat(j, 1) - amat(i, 1)), 2.0) + pow((amat(j, 2) - amat(i, 2)), 2.0));
    }

    out[i] = min(distance.elem(find(distance != 0)));

    if (! Progress::check_abort() ) {
      p.increment(); // update progress
    }
  }

  double min_val = min(out);

  return min_val;
}
