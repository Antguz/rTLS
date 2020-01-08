// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
using namespace Rcpp;

// Estimates the euclidean distance of a point in a 3D point cloud, and return a interger vector if the distance meet a given radius.

//[[Rcpp::export]]
Rcpp::IntegerVector distancelog(const NumericMatrix sample, const NumericMatrix base, const NumericVector radius) {

  int n = base.nrow();

  IntegerVector include(n);

  double distance;

  for (int i = 0; i < n; i++) {

    distance = std::sqrt((pow(base(i, 0) - sample(0, 0), 2.0) + pow(base(i, 1) - sample(0, 1), 2.0) + pow(base(i, 2) - sample(0, 2), 2.0)));

    if (distance == 0) {

      include[i] = 0;

    } else if (distance <= radius[0]) {

      include[i] = 1;

    } else if (distance >= radius[0]) {

      include[i] = 0;

    }

  }

  return include;
}
