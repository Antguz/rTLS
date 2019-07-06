#' @title Neighboring points based on knn
#'
#' @description Estimate the neighboring points of a targed point based on a knn.
#'
#' @param x A \code{data.frame} or \code{data.table} of a point with xyz coordinates.
#' @param cloud A \code{data.table} of a point cloud with xyz coordinates.
#' @param k An integer of a length 1 representing the number of neighbors to consider.
#' @param radius Optional. A \code{numeric} vector of a length 1 representing a priori radius from \code{x} to select the k nearest neighbors.   number of neighbors to consider. It speed up the calculations when \code{cloud} is too large.
#'
#' @return A \code{matrix} with the xyz coordinates of the k neighboring points and a fourth column with their distance.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @examples
#' data("pc_tree")
#' knn_neighbors(pc_tree[100,], pc_tree, k = 10)
#' knn_neighbors(pc_tree[100,], pc_tree, k = 10, radius = 0.5)
#'
#'@export
knn_neighbors <- function(targed, cloud, k, radius = NULL) {

  colnames(targed) <- c("X", "Y", "Z")
  colnames(cloud) <- c("X", "Y", "Z")

  xcoor <- targed$X[1] ###Set coordinates of interest
  ycoor <- targed$Y[1]
  zcoor <- targed$Z[1]

  if(is.null(radius) == TRUE) {

    space <- cloud[,1:3]

    space <- space[, distance := sqrt((xcoor - space$X)^2 + (ycoor - space$Y)^2 + (zcoor - space$Z)^2)] #Get the distance of the points
    space <- space[space$distance > 0,]
    space <- setorder(space, distance)

  } else if(is.null(radius) == FALSE){

    cube <- cloud[,1:3]

    cube <- cloud[between(X, xcoor - radius, xcoor + radius) & between(Y, ycoor - radius, ycoor + radius) & between(Z, zcoor - radius, zcoor + radius),] ###Set a cube to estimate the distance

    cube <- cube[,distance := sqrt((xcoor - cube$X)^2 + (ycoor - cube$Y)^2 + (zcoor - cube$Z)^2)] #Get the distance of the points in the cube
    space <- cube[cube$distance > 0,]
    space <- setorder(space, distance) #Order points by distance
  }

  space <- na.exclude(space[c(1:k),]) #Select the k points

  if(nrow(space) < 1) {
    return(space[1,])

  } else {
    return(space)
  }
}
