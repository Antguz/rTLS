#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix polar_to_cartesian_rcpp(NumericMatrix polar) {

  NumericMatrix cartesian(polar.nrow(), 3);

  static const double pi = 3.14159265;

  for (int i = 0; i < polar.nrow(); i++) {

    cartesian(i, 0) = polar(i, 2) * (cos((polar(i, 1)*pi)/180) * sin((polar(i, 0)*pi)/180));
    cartesian(i, 1) = polar(i, 2) * (sin((polar(i, 1)*pi)/180) * sin((polar(i, 0)*pi)/180));
    cartesian(i, 2) = polar(i, 2) * cos((polar(i, 0)*pi)/180);

  }

  return cartesian;
}
