#include <Rcpp.h>
#include "euclidean_distance.h"
#include <math.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix cartesian_to_polar_rcpp(NumericMatrix cartesian, NumericVector anchor) {

  NumericMatrix polar(cartesian.nrow(), 3);

  static const double pi = 3.14159265;

  NumericVector distance = euclidean_distance(anchor, cartesian);

  for (int i = 0; i < cartesian.nrow(); i++) {

   polar(i, 2) = distance[i]; //Distance

   //Zenith estimations

   double X = cartesian(i , 0);
   double Y = cartesian(i , 1);
   double Z = cartesian(i , 2);

   polar(i, 0) = (180 * acos((Z - anchor[2])/distance[i]))/pi; //Zenith angle estimation

   polar(i, 1) = (180 * atan2((Y - anchor[1]), (X - anchor[0])))/pi;

  }

  return polar;

}
