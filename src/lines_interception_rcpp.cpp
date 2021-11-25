#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppArmadillo"]]
// [[Rcpp::depends(RcppProgress)]]
#include <RcppArmadillo.h>
#include <progress.hpp>
#include <progress_bar.hpp>
#include <iostream>
#include "line_AABB_rcpp.h"

using namespace arma;

// [[Rcpp::export]]
arma::mat lines_interception_rcpp(arma::mat orig, arma::mat end, arma::mat voxels, arma::vec edge_length, int threads = 1, bool progress = true) {

  //Size of the loop
  int ng = voxels.n_rows;
  int nrays = orig.n_rows;

  //Create matrix of output
  arma::mat interceptions(ng, 9);

  //Parallel
#ifdef _OPENMP
  if ( threads > 0 ) {
    omp_set_num_threads( threads );
  }
#endif

  //Progress bar
  Progress p(ng, progress);

  //Loop on bounding boxs
#pragma omp parallel for
  for (int i = 0; i < ng; i++) {

    //Increment progress
    if (! Progress::check_abort() ) {
      p.increment(); // update progress
    }

    //Define min and max bounding box
    arma::vec voxel_min(3);
    arma::vec voxel_max(3);

    //Estimate the min and max bounding box
    //X
    voxel_min[0] = voxels(i, 0) - edge_length[0]/2;
    voxel_max[0] = voxels(i, 0) + edge_length[0]/2;
    //Y
    voxel_min[1] = voxels(i, 1) - edge_length[1]/2;
    voxel_max[1] = voxels(i, 1) + edge_length[1]/2;
    //Z
    voxel_min[2] = voxels(i, 2) - edge_length[2]/2;
    voxel_max[2] = voxels(i, 2) + edge_length[2]/2;

    int code_0 = 0; //Points are not intercepted
    int code_1 = 0; //Both ends falls inside
    int code_2 = 0; //Orig falls inside with exit
    int code_3 = 0; //Points with enter without exit
    int code_4 = 0; //Points with enter and exit

    double path_1 = 0; //Both ends falls inside
    double path_2 = 0; //Orig falls inside with exit
    double path_3 = 0; //Points with enter without exit
    double path_4 = 0; //Points with enter and exit

    //Loop on rays
    for (int j = 0; j < nrays; j++) {

      arma::mat origj = orig.row(j);
      arma::mat endj = end.row(j);

      //Estimate interception
      arma::vec ray(2);
      ray = line_AABB_rcpp(origj, endj, voxel_min, voxel_max);

      //Sum rays that are intercepted or not
      if(ray(0, 0) == 0) {
        code_0 = code_0 + 1;

      } else if(ray(0, 0) == 1) {
        code_1 = code_1 + 1;
        path_1 = path_1 + ray[1];

      } else if(ray(0, 0) == 2) {
        code_2 = code_2 + 1;
        path_2 = path_2 + ray[1];

      } else if(ray(0, 0) == 3) {
        code_3 = code_3 + 1;
        path_3 = path_3 + ray[1];

      } else if(ray(0, 0) == 4) {
        code_4 = code_4 + 1;
        path_4 = path_4 + ray[1];
      }
    }

    interceptions(i, 0) = code_0;
    interceptions(i, 1) = code_1;
    interceptions(i, 2) = code_2;
    interceptions(i, 3) = code_3;
    interceptions(i, 4) = code_4;
    interceptions(i, 5) = path_1;
    interceptions(i, 6) = path_2;
    interceptions(i, 7) = path_3;
    interceptions(i, 8) = path_4;
  }
  return interceptions;
}
