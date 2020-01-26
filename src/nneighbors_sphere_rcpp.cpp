#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppProgress)]]
#include <RcppArmadillo.h>
#include <progress.hpp>
#include <progress_bar.hpp>

using arma::sqrt;
using arma::pow;
using arma::sum;
using arma::sort_index;
using arma::find;
using namespace arma;

// [[Rcpp::export]]
arma::vec nneighbors_sphere_rcpp(arma::mat amat, double radius, int threads = 1, bool progress = true) {

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

    arma::vec nneighbors(an);

    for (int j = 0; j < an; j++) { //Loop to estimate the distance
      nneighbors(j) = sqrt(pow((amat(j, 0) - amat(i, 0)), 2.0) + pow((amat(j, 1) - amat(i, 1)), 2.0) + pow((amat(j, 2) - amat(i, 2)), 2.0));
    }

    arma::uvec ids = find(nneighbors > 0 && nneighbors <= radius);

    out(i) = ids.n_elem;

    if (! Progress::check_abort() ) {
      p.increment(); // update progress
    }
  }

  return out;
}
