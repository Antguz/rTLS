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
arma::mat dimensionality_rcpp(arma::mat amat, arma::mat bmat, double radius, int threads = 1) {

#ifdef _OPENMP
  if ( threads > 0 )
    omp_set_num_threads( threads );
    REprintf("Number of threads=%i\n", omp_get_max_threads());
#endif

  int an = amat.n_rows;
  int bn = bmat.n_rows;

  arma::mat out(an, 5);

  Progress p(an, true);

#pragma omp parallel for schedule(dynamic)
  for (int i = 0; i < an; i++) {

    arma::rowvec target = amat.row(i);
    arma::vec distance(bn);

    if (Progress::check_abort() ) {
      p.increment();

      for (int j = 0; j < bn; j++) { //Loop to estimate the distance

        //Progress bar

        arma::rowvec neighbour = bmat.row(j);

        distance(j) = sqrt(pow((neighbour[0] - target[0]), 2.0) + pow((neighbour[1] - target[1]), 2.0) + pow((neighbour[2] - target[2]), 2.0));
      }
    }

    arma::mat basemat(bmat.begin(), bn, 3, false);

    arma::mat basesub = basemat.rows(find(distance > 0 && distance <= radius));

    arma::mat covmat =  arma::cov(basesub);

    arma::vec eigenvalues =  arma::eig_sym(covmat);

    double total = sum(eigenvalues);

    out(i , 0) = i + 1;
    out(i , 1) = basesub.n_rows;
    out(i , 2) = eigenvalues[2]/total; //Row index of amat
    out(i , 3) = eigenvalues[1]/total; //Value of column 0 of bmat
    out(i , 4) = eigenvalues[0]/total; //Value of column 1 of bmat

  }

  return out;
}
