// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

// First it subsamble the matrix to specific rows, then it computes a cov matrix to extract eigenvalues

// [[Rcpp::export]]
arma::vec getEigenvalues(Rcpp::NumericMatrix base, Rcpp::NumericVector dis_logical) {

  mat basemat(base.begin(), base.nrow(), base.ncol(), false);

  colvec ID(dis_logical.begin(), dis_logical.size(), false);

  mat basesub = basemat.rows(find(ID == 1));

  mat covmat = cov(basesub, basesub);

  return arma::eig_sym(covmat);
}
