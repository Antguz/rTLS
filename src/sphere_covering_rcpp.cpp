#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppArmadillo"]]
// [[Rcpp::depends(RcppProgress)]]
#include <RcppArmadillo.h>
#include <progress.hpp>
#include <progress_bar.hpp>

using arma::sqrt;
using arma::pow;
using arma::sum;
using arma::find;
using namespace arma;

// [[Rcpp::export]]
arma::mat sphere_covering_rcpp(arma::mat amat, double radius, double kmax = 4, int threads = 1, bool progress = true) {

#ifdef _OPENMP
  if ( threads > 0 ) {
    omp_set_num_threads( threads );
    REprintf("Number of threads=%i\n", omp_get_max_threads());
  }
#endif

  int an = amat.n_rows;

  arma::mat out((an * kmax), 4, fill::zeros);

  Progress p(an, progress);

#pragma omp parallel for
  for (int i = 0; i < an; i++) {

    if (! Progress::check_abort() ) {
      p.increment(); // update progress
    }

    arma::vec distance(an); //Empy vector for distances
    int position = i * kmax; //Filling position for a given loop

    for (int j = 0; j < an; j++) { //Loop to estimate the distance

      double dx = (amat(j, 0) - amat(i, 0)); //Absolute values for diferences X

      if((dx > radius) || (dx < -radius)) {
        distance(j) = 0;

      } else {

        double dy = (amat(j, 1) - amat(i, 1)); //Absolute values for diferences Y

        if((dy > radius) || (dy < -radius)) {
          distance(j) = 0;

        } else {

          double dz = abs(amat(j, 2) - amat(i, 2)); //Absolute values for diferences Z

          if((dz > radius) || (dz < -radius)) {
            distance(j) = 0;

          } else {
            double euclidean = sqrt(pow(dx, 2.0) + pow(dy, 2.0) + pow(dz, 2.0));

            if(euclidean > radius) {
              distance(j) = 0;

            } else {
              distance(j) = euclidean;
            }
          }
        }
      }
    }

    arma::vec new_distance = distance.elem(find(distance > 0)); //Extraction of distance in a circunference

    int n_elements = new_distance.n_elem;

    if(n_elements > 0) { //If the vector have elements

      double min_distance = min(new_distance); //close neightbor distance

      arma::vec diameter = distance.elem(find(distance == min_distance)); //Diameter extraction
      arma::mat basesub = amat.rows(find(distance == min_distance)); //Position extraction

      int base_length = diameter.n_elem; //Base of rows or new rows

      out(position, 0) = (basesub(0, 0) + amat(i, 0))/2;
      out(position, 1) = (basesub(0, 1) + amat(i, 1))/2;
      out(position, 2) = (basesub(0, 2) + amat(i, 2))/2;
      out(position, 3) = diameter(0)/2;

      if(base_length > 1) {

        for(int k = 1; k < base_length; k++) {

          out((position + k), 0) = (basesub(k, 0) + amat(i, 0))/2;
          out((position + k), 1) = (basesub(k, 1) + amat(i, 1))/2;
          out((position + k), 2) = (basesub(k, 2) + amat(i, 2))/2;
          out((position + k), 3) = diameter(k)/2;

        }
      }
    }
  }

  return out;
}
