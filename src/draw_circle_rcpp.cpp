#include <RcppArmadillo.h>

using namespace arma;

// [[Rcpp::export]]
arma::mat draw_circle_rcpp(double X, double Y, double radius, int nvertices) {

  arma::mat circle(nvertices, 2);

  static const double pi = 3.14159265;

  arma::vec tt = linspace(0, 2*pi, nvertices);

  circle.col(0) = X + radius * cos(tt);
  circle.col(1) = Y + radius * sin(tt);

  return circle;

}
