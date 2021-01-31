#ifndef RAY_AABB_H
#define RAY_AABB_H

#include <RcppArmadillo.h>

int ray_AABB_rcpp(arma::mat orig, arma::mat dir, arma::vec voxel_min, arma::vec voxel_max);

#endif
