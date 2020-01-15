// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
#include <RcppArmadillo.h>

using namespace RcppParallel;
using namespace Rcpp;
using namespace arma;
using namespace std;

// [[Rcpp::depends(RcppParallel)]]
struct dimensionality : public Worker { //function object

  // input 3D-matrix
  const RMatrix<double> amat;
  const RMatrix<double> bmat;
  const RVector<double> radius;
  std::size_t row_size;
  std::size_t col_size;

  // output matrix to write to
  RMatrix<double> rmat;

  // initialize from Rcpp input and output matrixes
  dimensionality(const NumericMatrix amat, const NumericMatrix bmat, const NumericVector radius, NumericMatrix rmat, std::size_t row_size, std::size_t col_size)
    : amat(amat), bmat(bmat), radius(radius), rmat(rmat), row_size(row_size), col_size(col_size) {}

  //RMatrix and arma::mat convert
  arma::mat convertMatrix() {
    RMatrix<double> tmp_mat = bmat;
    arma::mat MAT(tmp_mat.begin(), row_size, col_size, false);
    return MAT;
  }

  //Function to get the eigenvalues
  arma::vec getEigenvalues(arma::mat base, arma::vec dis_logical) {

    arma::mat basemat(base.begin(), row_size, 3, false);

    arma::colvec ID(dis_logical.begin(), dis_logical.size(), false);

    arma::mat basesub = basemat.rows(find(ID == 1));

    arma::mat covmat = cov(basesub, basesub);

    return eig_sym(covmat);
  }

  // function call operator that work for the specified range (begin/end) #Not sure of this part
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {

      RMatrix<double>::Row amat_row = amat.row(i);

      arma::vec distance_logical(bmat.nrow());
      std::size_t point_size = 0;

      arma::mat MAT = convertMatrix();

      for (std::size_t j = 0; j < row_size; j++) { //Loop to estimate the distance

        RMatrix<double>::Row bmat_row = bmat.row(j);

        double distance = std::sqrt((pow(bmat_row[0] - amat_row[0], 2.0) + pow(bmat_row[1] - amat_row[1], 2.0) + pow(bmat_row[2] - amat_row[2], 2.0)));

        if (distance == 0) {
          distance_logical[i] = 0;

        } else if (distance <= radius[0]) {
          distance_logical[i] = 1;
          point_size = point_size + 1;

        } else if (distance >= radius[0]) {
          distance_logical[i] = 0;

        }
      }

      arma::vec eigenvalues = getEigenvalues(MAT, distance_logical);

      rmat(i , 0) = point_size;
      rmat(i , 1) = eigenvalues[2]; //Row index of amat
      rmat(i , 2) = eigenvalues[1]; //Value of column 0 of bmat
      rmat(i , 3) = eigenvalues[0]; //Value of column 1 of bmat

    }
  }
};

// [[Rcpp::export]]
NumericMatrix dimensionality_parallel(NumericMatrix amat, NumericMatrix bmat, NumericVector radius) {

  // allocate the matrix we will return
  NumericMatrix rmat(amat.nrow(), 4);
  std::size_t row_size = bmat.nrow();

  // create the worker
  dimensionality dimensionality(amat, bmat, radius, rmat, row_size, 3);

  // call it with parallelFor
  parallelFor(0, amat.nrow(), dimensionality);

  return rmat;
}
