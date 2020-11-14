#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppArmadillo"]]
#include <RcppArmadillo.h>

using namespace arma;

// [[Rcpp::export]]
arma::mat voxelization_rcpp(arma::mat cloud, double voxel_size, int threads = 1) {

#ifdef _OPENMP
  if ( threads > 0 ) {
    omp_set_num_threads( threads );
    }
#endif

  double xmin = min(cloud.col(0));
  double ymin = min(cloud.col(1));
  double zmin = min(cloud.col(2));

  arma::mat voxels(cloud.n_rows, 3);

#pragma omp parallel for
  for (int i = 0; i < cloud.n_rows; i++) {

    int xvox = floor(((cloud(i, 0) - xmin)/voxel_size));
    int yvox = floor(((cloud(i, 1) - ymin)/voxel_size));
    int zvox = floor(((cloud(i, 2) - zmin)/voxel_size));

    voxels(i, 0) = xmin + (xvox*voxel_size) + (voxel_size/2);
    voxels(i, 1) = ymin + (yvox*voxel_size) + (voxel_size/2);
    voxels(i, 2) = zmin + (zvox*voxel_size) + (voxel_size/2);

  }

  return voxels;
}
