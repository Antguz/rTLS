#ifndef ROTATE3D_H
#define ROTATE3D_H

#include <Rcpp.h>

NumericMatrix rotate3D_rcpp(NumericMatrix cloud, NumericVector roll, NumericVector pitch, NumericVector yaw, int threads = 1);

#endif
