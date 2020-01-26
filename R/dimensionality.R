#' @title Dimensionality of the Neighboring Points.
#'
#' @description Estimate the dimensionality of neighboring points in a cloud.
#'
#' @param cloud A \code{data.table} with *XYZ* coordinates in the first three columns.
#' @param cloud_b A \code{data.table} with *XYZ* coordinates in the first three columns to estimate the neighboring points. If \code{cloud_b} is \code{NULL}, \code{cloud_b == cloud}. \code{NULL} as default.
#' @param method A character string specifying the method to estimate the neighbors. It most be one of \code{"sphere"} or \code{"knn"}.
#' @param radius A \code{numeric} vector representing the radius of the sphere or spheres to consider. This needs be used if \code{method = "sphere"}.
#' @param k An \code{integer} vector representing the number of neighbors to consider. This needs be used if \code{method = "knn"}.
#' @param threads An \code{integer} specifying the number of threads to use for parallel processing. Experiment to see what works best for your data on your hardware.
#' @param progress Show progress. \code{TRUE} as default.
#'
#' @details The function returns the dimensionality of the neighboring points of a given point in \code{code}. The dimensionality is represented by the relative values of the
#' eigenvalues derived from a covariance matrix of the neighboring points.
#'
#' @return A \code{array} describing the point of the \code{cloud} in rows, the relative eigenvalues in columns, and the \code{radius} or \code{k} per slide. If \code{method = "sphere"}, it add in the first column the number of neighbor points.
#' @author J. Antonio Guzmán Q.
#'
#'
#' @examples
#' ###Estimate the dimensionality on a sample of 100 points.
#' #Load data
#' data("pc_tree")
#'
#' #Sample data
#' sample_data <- pc_tree[sample(nrow(pc_tree), 100), ]
#'
#' #Using neighbors in two spheres of 0.75 and 1
#' dimensionality(sample_data, pc_tree, method = "sphere", radius = c(0.75, 1))
#'
#' #Using two k neighbors of 50 and 100
#' dimensionality(cloud = sample_data, cloud_b = pc_tree, method = "knn", k = c(50, 100))
#'
#' @export
dimensionality <- function(cloud, cloud_b = NULL, method, radius, k, threads = 1, progress = TRUE) {

  if(method == "sphere") {
    if(is.null(cloud_b) == TRUE) {
      results <- dimensionality_sphere_rcpp(as.matrix(cloud), as.matrix(cloud), radius, threads, progress)
    } else {
      results <- dimensionality_sphere_rcpp(as.matrix(cloud), as.matrix(cloud_b), radius, threads, progress)
    }

    col_names <- c("npoints", "eig1", "eig2", "eig3")
    lev_names <- paste0("radius_", radius)

    results <- provideDimnames(results, base = list(as.character(seq_along(1:nrow(cloud))), col_names, lev_names))

  }  else if(method == "knn") {
    if(is.null(cloud_b) == TRUE) {
      results <- dimensionality_knn_rcpp(as.matrix(cloud), as.matrix(cloud), k, threads, progress)
    } else {
      results <- dimensionality_knn_rcpp(as.matrix(cloud), as.matrix(cloud_b), k, threads, progress)
    }

    col_names <- c("eig1", "eig2", "eig3")
    lev_names <- paste0("k_", k)

    results <- provideDimnames(results, base = list(as.character(seq_along(1:nrow(cloud))), col_names, lev_names))

  }

  return(results)
}
