#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector in_range(NumericVector x, double low, double high) {
  return x[x > low & x < high];
}
