#include <Rcpp.h>
#include "euclidean_distance.h"

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

   //Angles estimations

   if(X > anchor[0]) {
     if(Y > anchor[1]) {
       polar(i, 1) = (180 * atan((X-anchor[0])/(Y-anchor[1])))/pi; //Quadrant 1

     } else if(Y < anchor[1]) {
       polar(i, 1) = 180 - ((180 * atan((X-anchor[0])/(anchor[1]-Y)))/pi); //Quadrant 2

     } else if(Y == anchor[1]) { //Angle 90
       polar(i, 1) = 90;
     }

   } else if(X < anchor[0]) { //Quadrant 3
     if(Y < anchor[1]) {
       polar(i, 1) = 180 + ((180 * atan((anchor[0]-X)/(anchor[1]-Y)))/pi);

     } else if(Y > anchor[1]) { //Quadrant 4
       polar(i, 1) = 360 - ((180 * atan((anchor[1]-X)/(Y-anchor[2])))/pi);

     } else if(Y == anchor[1]) { //Angle 270
       polar(i, 1) = 270;

     }
   } else if(X == anchor[0]) {
     if(Y > anchor[1]) { //Angle 0
       polar(i, 1) = 0;

     } else if(Y < anchor[1]) { //Angle 180
       polar(i, 1) = 180;

     } else if(Y == anchor[1]) { //Angle 0
       polar(i, 1) = 0;

     }
   }
  }

  return polar;

}
