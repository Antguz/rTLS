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
#include "ray_AABB_rcpp.h"

using namespace arma;

// [[Rcpp::export]]
arma::mat rays_interception_rcpp(arma::mat orig, arma::mat dir, arma::mat voxels, arma::vec edge_length, int threads = 1, bool progress = true) {

  //Size of the loop
  int ng = voxels.n_rows;
  int nrays = orig.n_rows;

  //Create matrix of output
  arma::mat interceptions(ng, 3);

  //Parallel
#ifdef _OPENMP
  if ( threads > 0 ) {
    omp_set_num_threads( threads );
  }
#endif

  //Progress bar
  Progress p(ng*nrays, progress);

  //Loop on bounding boxs
#pragma omp parallel for
  for (int i = 0; i < ng; i++) {

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

    int non_enter = 0; //Points are not intercepted
    int enter_nonexit = 0; //Points are intercepted without exit
    int enter_exit = 0; //Points are intercepted with exit

    //Loop on rays
    for (int j = 0; j < nrays; j++) {

      //Increment progress
      if (! Progress::check_abort() ) {
        p.increment(); // update progress
      }

      arma::mat origj = orig.row(j);
      arma::mat dirj = dir.row(j);

      //Estimate interception
      int ray = ray_AABB_rcpp(origj, dirj, voxel_min, voxel_max);

      //Sum rays that are intercepted or not
      if(ray == 0) {
        non_enter = non_enter + 1;
      } else if(ray == 1) {
        enter_nonexit = enter_nonexit + 1;
      } else if(ray == 2) {
        enter_exit = enter_exit + 1;
      }
    }

    interceptions(i, 0) = non_enter;
    interceptions(i, 1) = enter_nonexit;
    interceptions(i, 2) = enter_exit;
  }

  return interceptions;

}
