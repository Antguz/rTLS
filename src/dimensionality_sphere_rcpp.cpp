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
using arma::sum;
using namespace arma;

// [[Rcpp::export]]
arma::cube dimensionality_rcpp(arma::mat amat, arma::mat bmat, arma::vec radius, int threads = 1) {

#ifdef _OPENMP
  if ( threads > 0 ) {
    omp_set_num_threads( threads );
    REprintf("Number of threads=%i\n", omp_get_max_threads());
  }
#endif

  int an = amat.n_rows;
  int bn = bmat.n_rows;
  int len_radius = radius.n_elem;

  arma::cube out(an, 8, len_radius);

  Progress p(an*len_radius, true);

#pragma omp parallel for
  for (int i = 0; i < an; i++) {

    arma::vec distance(bn);

    for (int j = 0; j < bn; j++) { //Loop to estimate the distance

      distance(j) = sqrt(pow((bmat(j, 0) - amat(i, 0)), 2.0) + pow((bmat(j, 1) - amat(i, 1)), 2.0) + pow((bmat(j, 2) - amat(i, 2)), 2.0));
    }

    for (int k = 0; k < len_radius; k++) {

      if (! Progress::check_abort() ) {
        p.increment(); // update progress
      }

      arma::mat basemat(bmat.begin(), bn, 3, false);

      arma::mat basesub = basemat.rows(find(distance > 0 && distance <= radius[k]));

      if(basesub.n_rows > 2) {

        arma::mat covmat =  arma::cov(basesub);

        arma::vec eigenvalues =  arma::eig_sym(covmat);

        double eigen_total =  sum(eigenvalues);
        double eig1 = eigenvalues[2]/eigen_total
        double eig2 = eigenvalues[1]/eigen_total
        double eig3 = eigenvalues[0]/eigen_total

        out(i , 0, k) = basesub.n_rows;
        out(i , 1, k) = eig1;
        out(i , 2, k) = eig2;
        out(i , 3, k) = eig3;
        out(i , 4, k) = eig1 - eig2;
        out(i , 5, k) = eig2 - eig3;
        out(i , 6, k) = (eig2 - eig3)/ eig1;
        out(i , 7, k) = (eig1 * log(eig1)) + (eig2 * log(eig2)) + (eig3 * log(eig3));
        out(i , 8, k) = (eig1 - eig2) / eig1;


      } else {

        out(i , 0, k) = basesub.n_rows;
        out(i , 1, k) = 0;
        out(i , 2, k) = 0;
        out(i , 3, k) = 0;
        out(i , 4, k) = 0;
        out(i , 5, k) = 0;
        out(i , 6, k) = 0;
        out(i , 7, k) = 0;
        out(i , 8, k) = 0;

      }

    }

  }

  return out;
}
