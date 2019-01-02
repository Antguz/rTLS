#' @import dplyr
#'
#' @title Point cloud filter
#'
#' @description Perform a different filtering methods to a point cloud.
#'
#' @param data A \code{matrix} or \code{data.frame} with xyz coordinates in the first three columns.
#' @param method A character string specifying the method to estimated the neighbors. It most be one of \code{"distance"} or \code{"knn"}.
#' @param radius A \code{numeric} vector of a length 1 representing the distance to consider. This will be used if \code{method = "distance"}.
#' @param k An integer of a length 1 representing the number of neighbors to consider. This will be used if \code{method = "knn"}.
#' @param parallel Logical, if \code{TRUE} it use a parallel processing.
#' @return A \code{data.frame} with the estimated parameters
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#' @references Wang, D., Hollaus, M., & Pfeifer, N. (2017). Feasibility of machine learning methods for separating wood and leaf points from Terrestrial Laser Scanning data. ISPRS Annals of Photogrammetry, Remote Sensing & Spatial Information Sciences, 4.
#'
#' @examples
#' data("pc_tree")
#'
#' ###Run from an object of class data.frame or matrix
#' cloud_dimensionality(pc_tree, method = "distance", radius = 0.2, parallel = FALSE)
#'
#' ###Run from an object of class neighborhood
#' dist <- neighborhood(pc_tree, method = "distance", radius = 0.2, parallel = FALSE)
#' cloud_dimensionality(dist, method = "distance", radius = 0.2, parallel = FALSE)
#'
#'@export
filter <- function(data, method, ) {
  method == "SOR" ### Statistical Outlier Removal (use distance or knn and sigma)
  method == "mnp" ### minimun number of points in a given distance (use distance and n)
  method == "by.value" ### filter by value (use value and column)
  method == "mean.distance" ### it use the mean distance as a thershold (use distance or knn and mean.thershold)

}
