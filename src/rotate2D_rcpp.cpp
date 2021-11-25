#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix rotate2D_rcpp(NumericMatrix plane, NumericVector angle, int threads = 1)  {

#ifdef _OPENMP
  if ( threads > 0 ) {
    omp_set_num_threads( threads );
  }
#endif

  NumericMatrix rotate(plane.nrow(), 2);

  static const double pi = 3.14159265;

  double angle_rad = (angle(0)*pi)/180; //Set the angles in radians

#pragma omp parallel for
  for (int i = 0; i < plane.nrow(); i++) { //Conduct the matrix multiplication

    rotate(i,0) = plane(i, 0)*cos(angle_rad) - plane(i, 1)*sin(angle_rad);
    rotate(i,1) = plane(i, 1)*cos(angle_rad) + plane(i, 0)*sin(angle_rad);

  }

  return rotate;
}
