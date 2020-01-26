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
using arma::sort_index;
using arma::find;
using namespace arma;

// [[Rcpp::export]]
arma::cube dimensionality_knn_rcpp(arma::mat amat, arma::mat bmat, arma::vec k, int threads = 1, bool progress = true) {

#ifdef _OPENMP
  if ( threads > 0 ) {
    omp_set_num_threads( threads );
    REprintf("Number of threads=%i\n", omp_get_max_threads());
  }
#endif

  int an = amat.n_rows;
  int bn = bmat.n_rows;
  int len_k = k.n_elem;

  arma::cube out(an, 3, len_k);

  Progress p(an*len_k, progress);

#pragma omp parallel for
  for (int i = 0; i < an; i++) {

    arma::vec distance(bn);

    for (int j = 0; j < bn; j++) { //Loop to estimate the distance

      distance(j) = sqrt(pow((bmat(j, 0) - amat(i, 0)), 2.0) + pow((bmat(j, 1) - amat(i, 1)), 2.0) + pow((bmat(j, 2) - amat(i, 2)), 2.0));
    }

    arma::uvec index = sort_index(distance);

    for (int m = 0; m < len_k; m++) {

      if (! Progress::check_abort() ) {
        p.increment(); // update progress
      }

      arma::mat basemat(bmat.begin(), bn, 3, false);

      arma::mat basesub = basemat.rows(find(index > 0 && index <= k[m]));

      arma::mat covmat =  arma::cov(basesub);

      arma::vec eigenvalues =  arma::eig_sym(covmat);

      double eigen_total =  sum(eigenvalues);

      out(i , 0, m) = eigenvalues[2]/eigen_total; //eigenvalue 1
      out(i , 1, m) = eigenvalues[1]/eigen_total; //eigenvalue 2
      out(i , 2, m) = eigenvalues[0]/eigen_total; //eigenvalue 3

    }
  }

  return out;
}
