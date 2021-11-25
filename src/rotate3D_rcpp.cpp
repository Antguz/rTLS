#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix rotate3D_rcpp(NumericMatrix cloud, NumericVector roll, NumericVector pitch, NumericVector yaw, int threads = 1)  {

#ifdef _OPENMP
  if ( threads > 0 ) {
    omp_set_num_threads( threads );
  }
#endif

  NumericMatrix rotate(cloud.nrow(), 3);

  static const double pi = 3.14159265;

  double rolla = (roll(0)*pi)/180; //Set the angles in radians
  double pitchb = (pitch(0)*pi)/180;
  double yawc = (yaw(0)*pi)/180;

  double cos_roll = cos(rolla); //Estimate the cos and sin
  double sin_roll = sin(rolla);

  double cos_pitch = cos(pitchb);
  double sin_pitch = sin(pitchb);

  double cos_yaw = cos(yawc);
  double sin_yaw = sin(yawc);

  double Axx = cos_yaw*cos_pitch; //Estimate the coeficients for the matrix multiplication
  double Axy = cos_yaw*sin_pitch*sin_roll - sin_yaw*cos_roll;
  double Axz = cos_yaw*sin_pitch*cos_roll + sin_yaw*sin_roll;

  double Ayx = sin_yaw*cos_pitch;
  double Ayy = sin_yaw*sin_pitch*sin_roll + cos_yaw*cos_roll;
  double Ayz = sin_yaw*sin_pitch*cos_roll - cos_yaw*sin_roll;

  double Azx = -sin_pitch;
  double Azy = cos_pitch*sin_roll;
  double Azz = cos_pitch*cos_roll;

#pragma omp parallel for
  for (int i = 0; i < cloud.nrow(); i++) { //Conduct the matrix multiplication

    rotate(i,0) = Axx*cloud(i, 0) + Axy*cloud(i, 1) + Axz*cloud(i, 2);
    rotate(i,1) = Ayx*cloud(i, 0) + Ayy*cloud(i, 1) + Ayz*cloud(i, 2);
    rotate(i,2) = Azx*cloud(i, 0) + Azy*cloud(i, 1) + Azz*cloud(i, 2);
  }

  return rotate;
}
