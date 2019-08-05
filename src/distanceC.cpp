#include <Rcpp.h>
using namespace Rcpp;

// Estimates the euclidean distance of a point in a 3D point cloud.

// [[Rcpp::export]]
NumericVector distanceC(double xcoor, double ycoor, double zcoor, NumericVector X, NumericVector Y, NumericVector Z) {
  int n = X.size();
  NumericVector out(n);

  for(int i = 0; i < n; ++i) {
    out[i] = sqrt((pow(X[i] - xcoor, 2.0) + pow(Y[i] - ycoor, 2.0) + pow(Z[i] - zcoor, 2.0)));
  }
  return out;
}
