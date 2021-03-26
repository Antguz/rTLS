#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppArmadillo"]]

#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>

using namespace arma;

// [[Rcpp::export]]
arma::mat circleRANSAC_rcpp(arma::mat cloud, double fpoints, double z_value, arma::vec poutlier, int max_iterations, int threads = 1) {

#ifdef _OPENMP
  if ( threads > 1 ) {
    omp_set_num_threads( threads );
  }
#endif

  int npoints = cloud.n_rows; //n of points in the cloud
  double npoints_double = npoints;
  int an_samples = round((npoints * fpoints)); //number of points for sample each iteration
  int int_outliers = round((npoints * poutlier(0))); //number of internal outliers
  int ext_outliers = round((npoints * poutlier(1))); //number of external outliers

  arma::mat xyr(max_iterations, 6); //Results to storage the iteration results

#pragma omp parallel for
  for(int i = 0; i < max_iterations; i++) { //loop of iterations

    arma::uvec samp = arma::randperm(npoints, an_samples); //select random number of samples
    arma::mat train_base = cloud.rows(samp); //subset values

    train_base.resize(an_samples, 3); //reside the matrix

    arma::vec equationcircle(an_samples); //create vector to compute the function

    for(int j = 0; j < an_samples; j++) { //compute the equation of the circle
      train_base(j, 2) = 1;
      equationcircle(j) = pow(train_base(j,0), 2) + pow(train_base(j,1), 2);
    }

    arma::vec solvefunction = arma::solve(train_base, equationcircle); //solve equation

    xyr(i,0) =  solvefunction(0)/2; //X coordinate
    xyr(i,1) =  solvefunction(1)/2; //Y coordinate
    xyr(i,2) =  sqrt(((pow( solvefunction(0) ,2) + pow(solvefunction(1) ,2)) / 4) + solvefunction(2)); //Solve function

    arma::vec residual(npoints);

    for(int k = 0; k < npoints; k++) { //compute the observed distances
      double observed_radio = sqrt(pow(cloud(k , 0) - xyr(i,0), 2) + pow(cloud(k, 1) - xyr(i,1), 2)); //Estimate the observed radio
      residual(k) = (observed_radio - xyr(i,2)); //residual
    }

    double threshold = z_value*(stddev(residual)/sqrt(npoints_double)); //Confidence of intervals
    double min_threshold = mean(residual) - threshold; //Create the minimum threshold
    double max_threshold = mean(residual) + threshold; //Create the maximum threshold

    int int_out = 0; //count inter outliers
    int ext_out = 0; //count external outliers
    double error = 0; //estimate error

    for(int l = 0; l < npoints; l++) { //compute the sum of squares
      if(residual(l) < min_threshold) {
        int_out = int_out + 1; //count number of internal outliers
      } else if(residual(l) > max_threshold) {
        ext_out = ext_out + 1; //count number of external outliers
      }
      error += pow(residual(l), 2);
    }

    xyr(i,3) = sqrt(error)/npoints_double; //errors per number of points
    xyr(i,4) = int_out; //Internal proportion of outliers
    xyr(i,5) = ext_out; //External proportion of outliers
  }

  arma::vec internal = xyr.col(4);
  xyr = xyr.rows(find(internal <= int_outliers)); //Subset by the internal

  arma::vec external = xyr.col(5);
  xyr = xyr.rows(find(external <= ext_outliers)); //Subset by the external

  double min_error = min(xyr.col(3));
  arma::vec sumoferror = xyr.col(3);
  xyr = xyr.rows(find(sumoferror == min_error)); //Subset by the lowest error

  xyr = xyr.cols(0, 3);

  return(xyr);
}
