#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix move_rotate_rcpp(NumericMatrix cloud, NumericVector move, NumericVector angles)  {

  NumericMatrix rotate(cloud.nrow(), 3);

  static const double pi = 3.14159265;

  double roll = (angles(0)*pi)/180; //Set the angles in radians
  double pitch = (angles(1)*pi)/180;
  double yaw = (angles(2)*pi)/180;

  double cos_roll = cos(roll); //Estimate the cos and sin
  double sin_roll = sin(roll);

  double cos_pitch = cos(pitch);
  double sin_pitch = sin(pitch);

  double cos_yaw = cos(yaw);
  double sin_yaw = sin(yaw);

  double Axx = cos_yaw*cos_pitch; //Estimate the coeficients for the matrix multiplication
  double Axy = cos_yaw*sin_pitch*sin_roll - sin_yaw*cos_roll;
  double Axz = cos_yaw*sin_pitch*cos_roll + sin_yaw*sin_roll;

  double Ayx = sin_yaw*cos_pitch;
  double Ayy = sin_yaw*sin_pitch*sin_roll + cos_yaw*cos_roll;
  double Ayz = sin_yaw*sin_pitch*cos_roll - cos_yaw*sin_roll;

  double Azx = -sin_pitch;
  double Azy = cos_pitch*sin_roll;
  double Azz = cos_pitch*cos_roll;

  for (int i = 0; i < cloud.nrow(); i++) { //Conduct the matrix multiplication

    double X_point = cloud(i, 0) - move(0);
    double Y_point = cloud(i, 1) - move(1);
    double Z_point = cloud(i, 2) - move(2);

    rotate(i,0) = Axx*X_point + Axy*Y_point + Axz*Z_point;
    rotate(i,1) = Ayx*X_point + Ayy*Y_point + Ayz*Z_point;
    rotate(i,2) = Azx*X_point + Azy*Y_point + Azz*Z_point;
  }

  return rotate;
}
