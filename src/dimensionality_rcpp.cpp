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

  arma::cube out(an, 4, len_radius);

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

        out(i , 0, k) = basesub.n_rows;
        out(i , 1, k) = eigenvalues[2]/sum(eigenvalues); //Row index of amat
        out(i , 2, k) = eigenvalues[1]/sum(eigenvalues); //Value of column 0 of bmat
        out(i , 3, k) = eigenvalues[0]/sum(eigenvalues); //Value of column 1 of bmat

      } else {

        out(i , 0, k) = basesub.n_rows;
        out(i , 1, k) = 0; //Row index of amat
        out(i , 2, k) = 0; //Value of column 0 of bmat
        out(i , 3, k) = 0; //Value of column 1 of bmat

      }

    }

  }

  return out;
}
