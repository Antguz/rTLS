// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

// First it subsamble the matrix to specific rows, then it computes a cov matrix to extract eigenvalues.

Rcpp::NumericVector getEigenvalues(Rcpp::NumericMatrix base, Rcpp::NumericVector dis_logical) {

  arma::mat basemat(base.begin(), base.nrow(), base.ncol(), false);

  arma::colvec ID(dis_logical.begin(), dis_logical.size(), false);

  arma::mat basesub = basemat.rows(find(ID == 1));

  arma::mat covmat = cov(basesub, basesub);

  arma::vec eigenvalues = eig_sym(covmat);

  return as<Rcpp::NumericVector>(wrap(eigenvalues));
}
