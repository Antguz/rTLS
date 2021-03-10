#' @title Euclidean Distance Between 3D points
#'
#' @description Estimate the distance between a point and a group of point.
#'
#' @param point A \code{numeric} vector of length three describing the *XYZ* coordinates.
#' @param cloud A \code{data.table} with *XYZ* coordinates in the first three columns representing a point cloud.
#' @param threads An \code{integer} specifying the number of threads to use for parallel processing. Experiment to see what works best for your data on your hardware.
#'
#'
#' @return A \code{numeric} vector describing of \code{point} to each row of \code{cloud}.
#' @author J. Antonio Guzmán Q.
#'
#' @examples
#' data("pc_tree")
#'
#' euclidean_distance(point = c(0, 0, 0), pc_tree)
#'
#' @export
euclidean_distance <- function(point, cloud, threads = 1L) {

  results <- euclidean_rcpp(point, as.matrix(cloud), threads)

  return(results)
}
