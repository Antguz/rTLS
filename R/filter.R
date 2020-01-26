#' Filtering of Point Clouds
#'
#' Filtering of point clouds using different methods
#'
#' @param cloud A \code{data.table} contain three columns representing the *XYZ* coordinates.
#' @param method A filtering method to use. It most be \code{"SOR"} or \code{"min_n"}.
#' @param radius A \code{numeric} vector representing the radius of the sphere to consider. This needs to be used if \code{method = "min_n"}.
#' @param min_n An \code{integer} representing the minimun number of neighbors to keep a given point. This needs to be used if \code{method = "min_n"}.
#' @param k An \code{integer} vector representing the number of neighbors to consider. This needs be used if \code{method = "SOR"}.
#' @param nSigma A \code{numeric} vector representing the standard deviation multiplier. This needs to be used if \code{method = "SOR"}.
#' @param threads An \code{integer} specifying the number of threads to use for parallel processing. Experiment to see what works best for your data on your hardware.
#' @param progress Show progress. \code{TRUE} as default.
#'
#' @return A \code{data.table} with the filtered points
#' @author J. Antonio Guzmán Q.
#'
#' @examples
#' #Load data
#' data("pc_tree")
#'
#' #Filter a sample of section of data using SOR
#' filter(pc_tree[1:5000,], method = "SOR", k = 20, nSigma = 1)
#'
#' #Filter using "min_n" of 1000
#' filter(pc_tree, method = "min_n", radius = 1.0, min_n = 5000)
#'
#' @export
filter <- function(cloud, method, radius, min_n, k, nSigma, threads = 1L, progress = TRUE) {

  if(method == "SOR") {

    mean_distance <- as.vector(meanDis_knn_rcpp(as.matrix(cloud), k, threads, progress))
    max_distance <- mean(mean_distance) + sd(mean_distance)*nSigma
    logic_sub <- mean_distance <= max_distance
    results <- cloud[logic_sub == TRUE, ]
  }

  if(method == "min_n") {

    neighbors <- as.vector(nneighbors_sphere_rcpp(as.matrix(cloud), radius, threads, progress))
    results <- cloud[neighbors >= min_n, ]

  }

  return(results)

}
