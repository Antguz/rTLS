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
double minimum_distance_rcpp(arma::mat amat, double radius, int threads = 1, bool progress = true) {

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

      double dx = (amat(j, 0) - amat(i, 0)); //Absolute values for differences X

      if((dx > radius) || (dx < -radius)) {
        distance(j) = 0;

      } else {

        double dy = (amat(j, 1) - amat(i, 1)); //Absolute values for differences Y

        if((dy > radius) || (dy < -radius)) {
          distance(j) = 0;

        } else {

          double dz = abs(amat(j, 2) - amat(i, 2)); //Absolute values for differences Z

          if((dz > radius) || (dz < -radius)) {
            distance(j) = 0;

          } else {
            double euclidean = sqrt(pow(dx, 2.0) + pow(dy, 2.0) + pow(dz, 2.0));

            if(euclidean > radius) {
              distance(j) = 0;

            } else {
              distance(j) = euclidean;
            }
          }
        }
      }
    }

    out[i] = min(distance.elem(find(distance != 0)));

    if (! Progress::check_abort() ) {
      p.increment(); // update progress
    }
  }

  double min_val = min(out);

  return min_val;
}
