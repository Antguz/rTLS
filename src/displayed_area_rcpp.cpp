#include <Rcpp.h>
using namespace Rcpp;

#include <boost/geometry.hpp>
#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/geometries/multi_polygon.hpp>

typedef boost::geometry::model::point<double, 2, boost::geometry::cs::cartesian> point;
typedef boost::geometry::model::polygon<point, true, true> polygon;
typedef boost::geometry::model::multi_polygon<polygon> multi_polygon_type;

namespace Rcpp {

  //Converter from R to Boost.Geometry's multi_polygon type
  template <> polygon as(SEXP pointsMatrixSEXP) {

    NumericMatrix pointsMatrix(pointsMatrixSEXP);
    polygon poly;

    for (int i = 0; i < pointsMatrix.nrow(); ++i) {
      double x = pointsMatrix(i,0);
      double y = pointsMatrix(i,1);
      point p(x,y);
      poly.outer().push_back(p);
    }

    return poly;
  }
}

// [[Rcpp::export]]
double displayed_area_rcpp(Rcpp::NumericMatrix sphere, NumericVector angles_vertices) {

  int points = sphere.nrow();
  int angles = angles_vertices.size();

  multi_polygon_type circles;

  for (int i = 0; i < sphere.nrow(); ++i) {

    NumericMatrix circle(angles, 3);

    for (int j = 0; j < angles; j++) { //Conduct the matrix multiplication

      circle(j, 0) = i + 1;
      circle(j, 1) = sphere(i, 2) * cos(angles_vertices(j)) + sphere(i, 0);
      circle(j, 2) = sphere(i, 2) * sin(angles_vertices(j)) + sphere(i, 1);

    }

    // Conversion of pointsMatrix here to boost::geometry polygon
    polygon poly = as<polygon>(circle);

    circles.push_back(poly);

  }

  double area = boost::geometry::area(circles);

  return area;
}
