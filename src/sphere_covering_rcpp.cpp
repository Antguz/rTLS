//This is an adaptation of FLANN C++ and rflann to the goals of rTLS

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo"]]
#include <string>
#include "flann/flann.hpp"

//[[Rcpp::export]]
arma::mat sphere_covering_rcpp(arma::mat query, int k , std::string build, int threads, int checks) {

  //Duplicate query
  arma::mat ref = query;

  k = k + 1;

  //Constant arguments
  const std::size_t n_dim = query.n_cols;
  const std::size_t n_query = query.n_rows;
  const std::size_t n_ref = ref.n_rows;

  // Column major to row major
  arma::mat qquery(n_dim, n_query);
  {
    arma::mat temp_q(query.begin(), n_query, n_dim, false);
    qquery = arma::trans(temp_q);
  }

  flann::Matrix<double> q_flann(qquery.memptr(), n_query, n_dim);

  arma::mat rref(n_dim, n_ref);
  {
    arma::mat temp_r(ref.begin(), n_ref, n_dim, false);
    rref = arma::trans(temp_r);
  }

  flann::Matrix<double> ref_flann(rref.memptr(), n_ref, n_dim);

  // Setting for FLANN
  flann::IndexParams params;
  if (build == "kdtree") {
    params = flann::KDTreeSingleIndexParams(1);
  } else if (build == "kmeans") {
    params = flann::KMeansIndexParams(2, 10, flann::FLANN_CENTERS_RANDOM, 0.2);
  } else if (build == "linear") {
    params = flann::LinearIndexParams();
  }

  // Finding the k nearest neighbors
  flann::Index<flann::L2<double> > index(ref_flann, params);
  index.buildIndex();
  flann::Matrix<int> indices_flann(new int[n_query * k], n_query, k);
  flann::Matrix<double> dists_flann(new double[n_query * k], n_query, k);

  //Parameters for search
  flann::SearchParams search_params;
  search_params.cores = threads;
  search_params.checks = checks;

  //Search of knn
  index.knnSearch(q_flann, indices_flann, dists_flann, k, search_params);
  arma::imat indices(indices_flann.ptr(), k, n_query, true);
  arma::mat dists(dists_flann.ptr(), k, n_query, true);

  //Delete arguments
  delete[] indices_flann.ptr();
  delete[] dists_flann.ptr();

  //Estimate distance if necessary
  int nrow = n_query*k;
  int k_row = 1;

  //Create matrix of results
  arma::mat results(nrow, 4);

  //Row index to save the values
  nrow = 0;

  //Loop to create long-format result
  for (int i = 0; i < n_query; i++) {

    k_row = 1;

    for (int j = 0; j < k; j++) {

      results(nrow, 0) = i + 1;
      results(nrow, 1) = indices(j, i) + 1;
      results(nrow, 2) = k_row;
      results(nrow, 3) = dists(j, i);

      nrow = nrow + 1;
      k_row = k_row + 1;
    }
  }

  arma::vec distance = results.col(3);
  results = results.rows(find(distance > 0));
  results.col(2) = results.col(2) - 1;

  //Creation of spheres---------------------------------------------------------

  //Rows
  int n_sphere = results.n_rows;

  //Create output
  arma::mat sphere(n_sphere, 5);

  //Loop to create long-format result
  for (int i = 0; i < n_sphere; i++) {

    int A = results(i, 0) - 1;
    int B = results(i, 1) - 1;

    sphere(i, 4) = results(i, 2);
    sphere(i, 3) = results(i, 3)/2;

    sphere(i, 0) = (query(A, 0) + query(B, 0))/2;
    sphere(i, 1) = (query(A, 1) + query(B, 1))/2;
    sphere(i, 2) = (query(A, 2) + query(B, 2))/2;
  }

  return sphere;
}
