#' @import dplyr
#'
#' @title An optimal voxel size, radius of a shpere, or k-neigboors.
#'
#' @description Estimate the optimal voxel size, radius of a sphere, or number of k neigboors for a given point cloud based on information theory.
#'
#' @param data A \code{matrix} or \code{data.frame} with xyz coordinates in the first three columns.
#' @param method A \code{character} describing the method to use. It most be one of \code{"voxels"}, \code{"radius"}, or \code{"knn"}.
#' @param size  A positive \code{numeric} vector to test a range of sizes and select the optimal size. This will be used if \code{method = "voxels"} and if \code{method = "radius"}.
#' @param k A positive \code{integer} vector to test a range of k-neigbors and select the optimal k. This will be used if \code{method = "knn"}.
#'
#'
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
optimal <- function(data, method, size, k, parallel) {

}
