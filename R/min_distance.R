#' @title Minimum Distance Between Points
#'
#' @description Estimate the minimum distance between points in a point cloud.
#'
#' @param cloud A \code{data.table} with *XYZ* coordinates in the first three columns representing a point cloud.
#' @param distance Type of distance to calculate. \code{"euclidean"} as default. Look \code{hnsw_knn} for more options.
#' @param threads An \code{integer} specifying the number of threads to use for parallel processing. Experiment to see what works best for your data on your hardware.
#' @param verbose If TRUE, log messages to the console.
#' @param progress If TRUE, log a progress bar when \code{verbose = TRUE}. Tracking progress could cause a small overhead.
#' @param ... Arguments passed to \code{hnsw_build} and \code{hnsw_search}.
#'
#' @return A \code{numeric} vector describing the minimum distance between points.
#' @author J. Antonio Guzmán Q.
#'
#' @examples
#' data("pc_tree")
#'
#' #Estimate the minimum distance of a sample o 100 points
#' min_distance(pc_tree)
#'
#' @export
min_distance <- function(cloud, distance = "euclidean", threads = 1L, verbose = FALSE, progress = FALSE, ...) {

  #test type of distance
  dist <- match.arg(distance, c("l2", "euclidean", "cosine", "ip"))
  results <- knn(cloud, cloud, k = 3, distance = dist, same = TRUE, threads = threads, ...)
  min_results <- min(results$distance)

  return(min_results)
}
