#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix rotate2D_rcpp(NumericMatrix plane, NumericVector angle)  {

  NumericMatrix rotate(plane.nrow(), 2);

  static const double pi = 3.14159265;

  double angle_rad = (angle(0)*pi)/180; //Set the angles in radians

  for (int i = 0; i < plane.nrow(); i++) { //Conduct the matrix multiplication

    rotate(i,0) = plane(i, 0)*cos(angle_rad) - plane(i, 1)*sin(angle_rad);
    rotate(i,1) = plane(i, 1)*cos(angle_rad) + plane(i, 0)*sin(angle_rad);

  }

  return rotate;
}
