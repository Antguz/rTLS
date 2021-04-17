#include <iostream>
#include <boost/polygon/polygon.hpp>

#include <vector>
#include "draw_circle_rcpp.h"

namespace bpl = boost::polygon;
typedef bpl::polygon_data<double> Polygon;
typedef bpl::polygon_traits<Polygon>::point_type Point;

// [[Rcpp::export]]
double displayed_area_rcpp(arma::mat spheres, int nvertices = 10L) {

  const std::size_t n_shperes = spheres.n_rows;

  // Create a polygon
  Polygon da;

  for (int i = 0; i < n_shperes; i++) {

    // Create a polygon
    Polygon polygon;

    double X = spheres(i, 0);
    double Y = spheres(i, 1);
    double radius = spheres(i, 2);

    arma::mat circle_sphere = draw_circle_rcpp(X, Y, radius, nvertices);

    // Convert to points
    std::vector<Point> points;

    for (int j = 0; j < nvertices; ++j) {

      double xx = circle_sphere(j, 0);
      double yy = circle_sphere(j, 1);

      points.push_back(Point(xx, yy));

    }

    polygon.set(points.begin(),points.end());

    boost::geometry::union_(da, polygon, da);

  }

  return bpl::area(da);
}
