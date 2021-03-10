#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace arma;
using std::min;
using std::max;

// [[Rcpp::export]]
arma::vec line_AABB_rcpp(arma::mat orig, arma::mat end, arma::vec AABB_min, arma::vec AABB_max) {

  arma::vec result(2);
  result(0) = 0;
  result(1) = 0;
  int feature = 0L;
  double path;

  //Early termination
  //X low
  if(orig(0, 0) <= AABB_min[0]) {
    if(end(0, 0) < orig(0, 0)) {
      return result;
    }
  }

  //X top
  if(orig(0, 0) >= AABB_max[0]) {
    if(end(0, 0) > orig(0, 0)) {
      return result;
    }
  }

  //Y low
  if(orig(0, 1) <= AABB_min[1]) {
    if(end(0, 1) < orig(0, 1)) {
      return result;
    }
  }
  //Y top
  if(orig(0, 1) >= AABB_max[1]) {
    if(end(0, 1) > orig(0, 1)) {
      return result;
    }
  }

  //Z low
  if(orig(0, 2) <= AABB_min[2]) {
    if(end(0, 2) < orig(0, 2)) {
      return result;
    }
  }
  //Z top
  if(orig(0, 2) >= AABB_max[2]) {
    if(end(0, 2) > orig(0, 2)) {
      return result;
    }
  }

  //If end point falls inside
  if((end(0, 0) >= AABB_min[0]) & (end(0, 0) < AABB_max[0])) {
    if((end(0, 1) >= AABB_min[1]) & (end(0, 1) < AABB_max[1])) {
      if((end(0, 2) >= AABB_min[2]) & (end(0, 2) < AABB_max[2])) {
        feature = 3L;
      }
    }
  }

  path = sqrt(pow(end(0, 0) - orig(0, 0), 2) + pow(end(0, 1) - orig(0, 1) , 2) + pow(end(0, 2) - orig(0, 2), 2));

  //If orig point falls inside
  if((orig(0, 0) >= AABB_min[0]) & (orig(0, 0) < AABB_max[0])) {
    if((orig(0, 1) >= AABB_min[1]) & (orig(0, 1) < AABB_max[1])) {
      if((orig(0, 2) >= AABB_min[2]) & (orig(0, 2) < AABB_max[2])) {

        //If end point also falls inside
        if(feature == 3L) {

          feature = 1L;
          result(0) = feature;
          result(1) = path;
          return result;
        }

        double x;
        double y;
        double z;
        feature = 2L;

        if(end(0, 0) < AABB_min[0]) {
          x = AABB_min[0];
        } else if(end(0, 0) > AABB_max[0]) {
          x = AABB_max[0];
        } else {
          x = end(0, 0);
        }

        if(end(0, 1) < AABB_min[1]) {
          y = AABB_min[1];
        } else if(end(0, 1) > AABB_max[1]) {
          y = AABB_max[1];
        } else {
          y = end(0, 1);
        }

        if(end(0, 2) < AABB_min[2]) {
          z = AABB_min[1];
        } else if(end(0, 2) > AABB_max[2]) {
          z = AABB_max[2];
        } else {
          z = end(0, 2);
        }

        path = sqrt(pow(x - orig(0, 0), 2) + pow(y - orig(0, 1) , 2) + pow(z - orig(0, 2), 2));
        result(0) = feature;
        result(1) = path;
        return result;
      }
    }
  }

  if(feature == 3L) {

    double x;
    double y;
    double z;
    feature = 3L;

    if(orig(0, 0) < AABB_min[0]) {
      x = AABB_min[0];
    } else if(orig(0, 0) > AABB_max[0]) {
      x = AABB_max[0];
    } else {
      x = orig(0, 0);
    }

    if(orig(0, 1) < AABB_min[1]) {
      y = AABB_min[1];
    } else if(orig(0, 1) > AABB_max[1]) {
      y = AABB_max[1];
    } else {
      y = orig(0, 1);
    }

    if(orig(0, 2) < AABB_min[2]) {
      z = AABB_min[1];
    } else if(orig(0, 2) > AABB_max[2]) {
      z = AABB_max[2];
    } else {
      z = orig(0, 2);
    }

    path = sqrt(pow(x - end(0, 0), 2) + pow(y - end(0, 1) , 2) + pow(z - end(0, 2), 2));
    result(0) = feature;
    result(1) = path;
    return result;

  }

  //Vector length dimension of x, y, and z
  double dir_x = end(0, 0) - orig(0, 0);
  double dir_y = end(0, 1) - orig(0, 1);
  double dir_z = end(0, 2) - orig(0, 2);

  //lb is the corner of AABB with minimal coordinates - left bottom,
  //rt is maximal corner
  //r.org is origin of ray
  double txmin = (AABB_min[0] - orig(0, 0)) / dir_x;
  double txmax = (AABB_max[0] - orig(0, 0)) / dir_x;

  double tmin = min(txmin, txmax);
  double tmax = max(txmin, txmax);

  double tymin = (AABB_min[1] - orig(0, 1)) / dir_y;
  double tymax = (AABB_max[1]- orig(0, 1)) / dir_y;

  tmin = max(tmin, min(tymin, tymax));
  tmax = min(tmax, max(tymin, tymax));

  //Estimate on z
  double tzmin = (AABB_min[2] - orig(0, 2)) / dir_z;
  double tzmax = (AABB_max[2] - orig(0, 2)) / dir_z;

  //Tmin and tmax to use
  tmin = max(tmin, min(tzmin, tzmax));
  tmax = min(tmax, max(tzmin, tzmax));

  if(tmin > 1 || tmax > 1) {
    return result;
  }

  if(tmin > tmax) {
    return result;
  }

  result(0) = 4L;
  result(1) = (path*tmax) - (path*tmin);
  return result;

}
