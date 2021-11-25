#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppArmadillo"]]
// [[Rcpp::depends(RcppProgress)]]
#include <RcppArmadillo.h>
#include <progress.hpp>
#include <progress_bar.hpp>

// [[Rcpp::export]]
arma::cube features_knn_rcpp(arma::mat index, arma::mat query, arma::vec k, int threads = 1, bool progress = true) {

//Set threads
#ifdef _OPENMP
  if ( threads > 0 ) {
    omp_set_num_threads( threads );
    REprintf("Number of threads=%i\n", omp_get_max_threads());
  }
#endif

  //query row selection
  arma::vec query_row = index.col(0);

  //set unique for loops
  arma::uvec query_unique = find_unique(query_row);
  arma::vec index_query = query_row.elem(query_unique); //unique query

  //Length of the cube in the 0 dimension
  int an = index_query.n_elem;

  //Length of the cube in the 2 dimension
  int len_k = k.n_elem;

  //output cube
  arma::cube out(an, 3, len_k);

  //create progress
  Progress p(an*len_k, progress);

#pragma omp parallel for
  for (int i = 0; i < an; i++) {

    //Subset per query index first
    arma::mat sub_index = index.rows(find(query_row == index_query[i]));

    //k index
    arma::vec index_k = sub_index.col(2);

    for (int m = 0; m < len_k; m++) {

      if (! Progress::check_abort() ) {
        p.increment(); // update progress
      }

      //Subset ref index based on k
      arma::mat ref = sub_index.rows(find(index_k <= k[m]));

      int npoints = ref.n_rows;

      if(npoints > 3) {

        //Select ref index
        arma::vec index_ref = ref.col(1);

        //Create uvec
        arma::uvec ids_ref = arma::conv_to< arma::uvec >::from(index_ref);

        //Subset points of ref values
        arma::mat points = query.rows(ids_ref);

        //Estimate the cov matrix
        arma::mat covmat =  arma::cov(points);

        //Estimate eigen vectors
        arma::vec eigenvalues =  arma::eig_sym(covmat);

        double eigen_total = sum(eigenvalues);

        out(i , 0, m) = eigenvalues[2]/eigen_total; //eigenvalue 1
        out(i , 1, m) = eigenvalues[1]/eigen_total; //eigenvalue 2
        out(i , 2, m) = eigenvalues[0]/eigen_total; //eigenvalue 3

      } else {

        out(i , 0, m) = R_NaN; //eigenvalue 1
        out(i , 1, m) = R_NaN; //eigenvalue 2
        out(i , 2, m) = R_NaN; //eigenvalue 3

      }
    }
  }

  return out;
}
