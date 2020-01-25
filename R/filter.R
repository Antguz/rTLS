#' Filtering of Point Clouds
#'
#' Filtering of point clouds using different methods
#'
#' @param cloud A \code{data.table} contain three columns representing the *XYZ* coordinates.
#' @param method A positive \code{numeric} vector describing the number of point clouds to use.
#' @param radius A \code{numeric} vector representing the radius of the sphere or spheres to consider. This needs to be used if \code{method = "min_neighbors"}.
#' @param k An \code{integer} vector representing the number of neighbors to consider. This needs be used if \code{method = "SOR"}.
#' @param nSigma A \code{numeric} vector representing the standard deviation multiplier. This needs to be used if \code{method = "SOR"}.
#' @param threads An \code{integer} specifying the number of threads to use for parallel processing. Experiment to see what works best for your data on your hardware.
#' @param progress Show progress. \code{TRUE} as default.
#'
#' @return A \code{data.table} with the filtered points
#' @author J. Antonio Guzmán Q.
#'
#' @examples
#' #Import an example point cloud
#' path <- system.file("extdata", "pc_tree.txt", package = "rTLS")
#'
#' #Creates a stand of 4 trees with 10% of overlap
#' files <- rep(path, 50)
#' artificial_stand(files, n.trees = 50, dimension = c(50, 50), overlap = 10)
#'
#' #Creates a stand of 4 trees with their locations
#' location <- data.table(X = c(5, 10, 10, 5), Y = c(5, 5, 10, 10))
#' artificial_stand(files, n.trees = 4, dimension = c(15, 15), coordinates = location)
#'
#' @export
filter <- function(cloud, method, radius, k, nSigma, threads = 1, progress = TRUE) {

  if(method == "SOR") {

    mean_distance <- meanDis_knn_rcpp(as.matrix(cloud), k, threads, progress)
    average_distance <- mean(mean_distance)
    max_distance <- mean(mean_distance) + sd(mean_distance)*nSigma

  } else if(method == "min_n") {


  }

}
