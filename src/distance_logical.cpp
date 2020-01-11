// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>

using namespace Rcpp;

NumericVector distance_logical(const NumericVector sample, const NumericMatrix base, const NumericVector radius) {

  int n = base.nrow();

  NumericVector include(n);

  double distance;

  for (int i = 0; i < n; i++) {

    distance = std::sqrt((pow(base(i, 0) - sample(0), 2.0) + pow(base(i, 1) - sample(1), 2.0) + pow(base(i, 2) - sample(2), 2.0)));

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
