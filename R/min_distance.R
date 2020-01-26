#' @title Minimun Distance Between Points
#'
#' @description Estimate the minimum distance between points in a point cloud.
#'
#' @param cloud A \code{data.table} with *XYZ* coordinates in the first three columns representing a point cloud.
#' @param threads An \code{integer} specifying the number of threads to use for parallel processing. Experiment to see what works best for your data on your hardware.
#' @param progress Show progress. \code{TRUE} as default.
#'
#' @return A \code{numeric} vector describing the minimun distance between points.
#' @author J. Antonio Guzmán Q.
#'
#' @examples
#' data("pc_tree")
#'
#' #Estimate the minimun distance of a sample o 100 points
#' cloud_sample <- pc_tree[sample(nrow(pc_tree), 100), ]
#' min_distance(cloud_sample)
#'
#' @export
min_distance <- function(cloud, threads = 1, progress = TRUE) {

  results <- minimun_distance_rcpp(as.matrix(cloud), threads, progress)

  return(results)
}
