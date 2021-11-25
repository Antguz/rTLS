#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppArmadillo"]]
#include <RcppArmadillo.h>

using namespace arma;

// [[Rcpp::export]]
arma::mat voxelization_rcpp(arma::mat cloud, arma::vec edge_length, int threads = 1) {

#ifdef _OPENMP
  if ( threads > 0 ) {
    omp_set_num_threads( threads );
  }
#endif

  double xmin = min(cloud.col(0));
  double ymin = min(cloud.col(1));
  double zmin = min(cloud.col(2));

  int nrowspc = cloud.n_rows;

  arma::mat voxels(nrowspc, 3);

#pragma omp parallel for
  for (int i = 0; i < nrowspc; i++) {

    int xvox = floor(((cloud(i, 0) - xmin)/edge_length[0]));
    int yvox = floor(((cloud(i, 1) - ymin)/edge_length[1]));
    int zvox = floor(((cloud(i, 2) - zmin)/edge_length[2]));

    voxels(i, 0) = xmin + (xvox*edge_length[0]) + (edge_length[0]/2);
    voxels(i, 1) = ymin + (yvox*edge_length[1]) + (edge_length[1]/2);
    voxels(i, 2) = zmin + (zvox*edge_length[2]) + (edge_length[2]/2);

  }

  return voxels;
}
