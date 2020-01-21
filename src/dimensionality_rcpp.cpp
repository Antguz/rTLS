// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

using arma::sqrt;
using arma::pow;
using namespace arma;

// [[Rcpp::export]]
arma::mat dimensionality_rcpp(arma::mat amat, arma::mat bmat, double radius) {

  int an = amat.n_rows;
  int bn = bmat.n_rows;

  arma::mat out(an, 4);

  for (int i = 0; i < an; i++) {

    arma::vec target = amat.row(i);
    arma::vec distance(bn);

    for (int j = 0; j < bn; j++) { //Loop to estimate the distance

      arma::vec neighbour = bmat.row(j);

      distance(j) = sqrt(pow((neighbour[0] - target[0]), 2.0) + pow((neighbour[1] - target[1]), 2.0) + pow((neighbour[2] - target[2]), 2.0));
    }

    arma::mat basemat(bmat.begin(), bn, 3, false);

    arma::mat basesub = basemat.rows(find(distance > 0 && distance <= radius));

    arma::mat covmat = cov(basesub);

    arma::vec eigenvalues = eig_sym(covmat);

    out(i , 0) = i;
    out(i , 1) = eigenvalues[2]; //Row index of amat
    out(i , 2) = eigenvalues[1]; //Value of column 0 of bmat
    out(i , 3) = eigenvalues[0]; //Value of column 1 of bmat

  }

  return out;
}
