#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace arma;
using std::min;
using std::max;

// [[Rcpp::export]]
int ray_AABB_rcpp(arma::mat orig, arma::mat dir, arma::vec voxel_min, arma::vec voxel_max) {

  //If end point falls inside
  if(dir(0, 0) >= voxel_min[0] & dir(0, 0) < voxel_max[0]) {
    if(dir(0, 1) >= voxel_min[1] & dir(0, 1) < voxel_max[1]) {
      if(dir(0, 2) >= voxel_min[2] & dir(0, 2) < voxel_max[2]) {
        return 1;
      }
    }
  }

  //If orig point falls inside
  if(orig(0, 0) >= voxel_min[0] & orig(0, 0) < voxel_max[0]) {
    if(orig(0, 1) >= voxel_min[1] & orig(0, 1) < voxel_max[1]) {
      if(orig(0, 2) >= voxel_min[2] & orig(0, 2) < voxel_max[2]) {
        if(dir(0, 0) >= voxel_min[0] || dir(0, 0) < voxel_max[0]) {
          return 2;
        }
        if(dir(0, 2) >= voxel_min[2] || dir(0, 2) < voxel_max[2]) {
          return 2;
        }
        if(dir(0, 2) >= voxel_min[2] || dir(0, 2) < voxel_max[2]) {
          return 2;
        }
      }
    }
  }

  //Vector length dimension of x and y
  double dir_x = dir(0, 0) - orig(0, 0);
  double dir_y = dir(0, 1) - orig(0, 1);
  double dir_z = dir(0, 2) - orig(0, 2);

  //lb is the corner of AABB with minimal coordinates - left bottom,
  //rt is maximal corner
  //r.org is origin of ray
  double txmin = (voxel_min[0] - orig(0, 0)) / dir_x;
  double txmax = (voxel_max[0] - orig(0, 0)) / dir_x;

  double tmin = min(txmin, txmax);
  double tmax = max(txmin, txmax);

  double tymin = (voxel_min[1] - orig(0, 1)) / dir_y;
  double tymax = (voxel_max[1]- orig(0, 1)) / dir_y;

  tmin = max(tmin, min(tymin, tymax));
  tmax = min(tmax, max(tymin, tymax));

  //Estimate on z
  double tzmin = (voxel_min[2] - orig(0, 2)) / dir_z;
  double tzmax = (voxel_max[2] - orig(0, 2)) / dir_z;

  tmin = max(tmin, min(tzmin, tzmax));
  tmax = min(tmax, max(tzmin, tzmax));

  if((tmin < tmax)) {
    if(tmax > 1) {
      return 0;
    }
  }

  if(tmin > tmax) {
    return 0;
  }

  if(tmin < 1 & tmax < 1) {
    if(tmax > 1) {
      return 0;
    }
  } else {
    return 2;
  }

  return 2;
}
