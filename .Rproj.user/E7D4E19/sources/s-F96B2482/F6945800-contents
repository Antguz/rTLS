#' @import dplyr
#'
#' @title Neighboring points based on knn
#'
#' @description Estimate the neighboring points of a targed point based on a knn
#'
#' @param x a \code{matrix} or \code{data.frame} of a point with xyz coordinates
#' @param cloud a \code{matrix} or \code{data.frame} of a point cloud with xyz coordinates
#' @param k an integer of a length 1 representing the number of neighbors to consider
#'
#'
#' @return A \code{matrix} with the xyz coordinates of the k neighboring points and a fourth column with their distance
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @examples
#' data("pc_tree")
#' knn_neighbors(pc_tree[100,], pc_tree, k = 10)
#'
#'@export
knn_neighbors <- function(x, cloud, k) {
  n_knn <- knn(data = as.matrix(cloud[,1:3]) , query = as.matrix(x[,1:3]), k = 10+1, eps = 0, searchtype = 1L, radius = 0)
  kpoints <- as.vector(n_knn$nn.idx)[2:(k+1)]
  distance <- as.vector(n_knn$nn.dists)[2:(k+1)]
  cube <- as.data.frame(cloud[kpoints, 1:3])
  cube$distance <- distance
  cube <- as.matrix(cube)
  return(cube)
}
