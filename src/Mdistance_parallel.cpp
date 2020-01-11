// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
#include "distance_logical.h"
#include "getEigenvalues.h"

using namespace std;
using namespace Rcpp;
using namespace RcppParallel;

//' @title Dimensionality
//'
//' @description First it subsamble the matrix to specific rows, then it computes a cov matrix to extract eigenvalues
//'
//' @param amat A matrix
//' @param bmat A matrix
//' @param radius A numeric vector
//' @author J. Antonio Guzmán Q.
//'
//' @useDynLib rTLS
//' @importFrom Rcpp sourceCpp
//'
//' @return A matrix
//'
// [[Rcpp::depends(RcppParallel)]]
struct Mdistance : public Worker { //function object

  // input 3D-matrix
  const RMatrix<double> amat;
  const RMatrix<double> bmat;
  const RVector<double> radius;

  // output matrix to write to
  RMatrix<double> rmat;

  // initialize from Rcpp input and output matrixes
  Mdistance(const NumericMatrix amat, const NumericMatrix bmat, const NumericVector radius, NumericMatrix rmat)
    : amat(amat), bmat(bmat), radius(radius), rmat(rmat) {}

  int n = bmat.nrow();

  // function call operator that work for the specified range (begin/end) #Not sure of this part
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {

      RVector<int> distance_logical(n);

      for (int j = 0; j < n; i++) { //Loop to estimate the distance

        double distance = std::sqrt((pow(bmat(j, 0) - amat(i, 0), 2.0) + pow(bmat(j, 1) - amat(i, 1), 2.0) + pow(bmat(j, 2) - amat(i, 2), 2.0)));

        if (distance == 0) {
          distance_logical[i] = 0;

        } else if (distance <= radius[0]) {
          distance_logical[i] = 1;

        } else if (distance >= radius[0]) {
          distance_logical[i] = 0;

        }
      }

      //Estimation of the covariance matrix

      NumericVector eigenvalues = getEigenvalues(bmat, distance_logical);

      NumericVector xx = NumericVector::create(eigenvalues[0] + eigenvalues[1] + eigenvalues[2]);

      rmat(i , 0) = eigenvalues[2]; //Row index of amat
      rmat(i , 1) = eigenvalues[1]; //Value of column 0 of bmat
      rmat(i , 2) = eigenvalues[0]; //Value of column 1 of bmat

    }
  }
};

// [[Rcpp::export]]
NumericMatrix Mdistance_parallel(NumericMatrix amat, NumericMatrix bmat, NumericVector radius) {

  // allocate the matrix we will return
  NumericMatrix rmat(amat.nrow(), 3);

  // create the worker
  Mdistance Mdistance(amat, bmat, radius, rmat);

  // call it with parallelFor
  parallelFor(0, amat.nrow(), Mdistance);

  return as<NumericMatrix>(wrap(rmat));
}
