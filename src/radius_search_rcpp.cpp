//This is an adaptation of FLANN C++ and rflann to the goals of rTLS

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo"]]
#include <vector>
#include <string>
#include "flann/flann.hpp"

//[[Rcpp::export]]
arma::mat radius_search_rcpp(arma::mat query, arma::mat ref, double radius, int max_neighbour, bool same, std::string build, int threads, int checks) {

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

  // Finding the neighbors based on a radius
  flann::Index<flann::L2<double> > index(ref_flann, params);
  index.buildIndex();
  std::vector< std::vector<int> >
    indices_flann(n_query, std::vector<int>(max_neighbour));
  std::vector< std::vector<double> >
    dists_flann(n_query, std::vector<double>(max_neighbour));

  //Parameters for search
  flann::SearchParams search_params;
  search_params.cores = threads;
  search_params.checks = checks;
  search_params.max_neighbors = max_neighbour;

  //Radius search
  index.radiusSearch(q_flann, indices_flann, dists_flann, radius, search_params);

  //Estimate distance if necessary
  int nrow = n_query*max_neighbour;

  //Create matrix of results
  arma::mat results(nrow, 3, arma::fill::zeros);

  //Row index to save the values
  nrow = 0;

  //Length of the nested vector
  int vlength = 0;

  //Loop to create long-format result
  for (int i = 0; i < n_query; i++) {

    vlength = indices_flann[i].size();

    if(vlength != 0) {

      for (int j = 0; j < vlength; j++) {

        results(nrow, 0) = i + 1;
        results(nrow, 1) = indices_flann[i][j] + 1;
        results(nrow, 2) = dists_flann[i][j];

        nrow = nrow + 1;
      }
    }
  }

  arma::vec keep = results.col(0);
  arma::mat final = results.rows(find(keep > 0));

  if(same == true) {
    arma::vec distance = final.col(2);
    final = final.rows(find(distance > 0));
  }

  return final;
}
