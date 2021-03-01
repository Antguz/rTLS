#' @title Minimum Distance Between Points
#'
#' @description Estimate the minimum distance between points in a point cloud.
#'
#' @param cloud A \code{data.table} with *XYZ* coordinates in the first three columns representing a point cloud.
#' @param threads An \code{integer} specifying the number of threads to use for parallel processing. Experiment to see what works best for your data on your hardware.
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
min_distance <- function(cloud, threads = 1L) {

  results <- knn(cloud, cloud, k = 3, same = TRUE, build = "kdtree", threads = threads, checks = 10)
  min_results <- min(results$distance)

  return(min_results)
}
