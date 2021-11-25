#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix cartesian_to_polar_rcpp(NumericMatrix cartesian, NumericVector anchor, int threads = 1) {

#ifdef _OPENMP
   if ( threads > 0 ) {
      omp_set_num_threads( threads );
   }
#endif

   NumericMatrix polar(cartesian.nrow(), 3);

   static const double pi = 3.14159265;

#pragma omp parallel for
   for (int i = 0; i < cartesian.nrow(); i++) {

      double X = cartesian(i , 0);
      double Y = cartesian(i , 1);
      double Z = cartesian(i , 2);

      //Distance

      double distance = sqrt((pow(X - anchor[0], 2.0) + pow(Y - anchor[1], 2.0) + pow(Z - anchor[2], 2.0)));

      polar(i, 2) = distance;

      //Zenith estimations

      polar(i, 0) = (180 * acos((Z - anchor[2])/distance))/pi;

      //Azimuth

      polar(i, 1) = (180 * atan2((Y - anchor[1]), (X - anchor[0])))/pi;

   }

   return polar;

}
