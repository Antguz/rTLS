// Copyright 2015 <Jeremy Yee> <jeremyyee@outlook.com.au>
// Nearest Neighbours
////////////////////////////////////////////////////////////////////////////////

#include <RcppArmadillo.h>
#include <string>
#include "flann/flann.hpp"

//[[Rcpp::export]]
Rcpp::List Neighbour(Rcpp::NumericMatrix query,
                     Rcpp::NumericMatrix ref,
                     int k,
                     std::string build,
                     int cores,
                     int checks) {
  const std::size_t n_dim = query.ncol();
  const std::size_t n_query = query.nrow();
  const std::size_t n_ref = ref.nrow();
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
  // Setting the flann index params
  flann::IndexParams params;
  if (build == "kdtree") {
    params = flann::KDTreeSingleIndexParams(1);
  } else if (build == "kmeans") {
    params = flann::KMeansIndexParams(2, 10, flann::FLANN_CENTERS_RANDOM, 0.2);
  } else if (build == "linear") {
    params = flann::LinearIndexParams();
  }
  // Finding the nearest neighbours
  flann::Index<flann::L2<double> > index(ref_flann, params);
  index.buildIndex();
  flann::Matrix<int> indices_flann(new int[n_query * k], n_query, k);
  flann::Matrix<double> dists_flann(new double[n_query * k], n_query, k);
  flann::SearchParams search_params;
  search_params.cores = cores;
  search_params.checks = checks;
  index.knnSearch(q_flann, indices_flann, dists_flann, k, search_params);
  arma::imat indices(indices_flann.ptr(), k, n_query, true);
  arma::mat dists(dists_flann.ptr(), k, n_query, true);
  delete[] indices_flann.ptr();
  delete[] dists_flann.ptr();
  return Rcpp::List::create(Rcpp::Named("indices") = indices.t() + 1,
                            Rcpp::Named("distances") = dists.t());
}
