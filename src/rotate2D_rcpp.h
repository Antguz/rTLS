#ifndef ROTATE2D_H
#define ROTATE2D_H

#include <Rcpp.h>

NumericMatrix rotate2D_rcpp(NumericMatrix plane, NumericVector angle, int threads = 1);

#endif
