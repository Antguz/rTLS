#ifndef DISTANCELOG_HPP
#define DISTANCELOG_HPP

#include <Rcpp.h>

Rcpp::IntegerVector distancelog(const NumericMatrix sample, const NumericMatrix base, const NumericVector radius);

#endif
