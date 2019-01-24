#' @title k-nearest neighbors metrics on a target point
#'
#' @description Estimate different metrics on a neighborhood of a targed point using knn.
#'
#' @param x  A \code{data.table} or \code{data.frame} of a targed point with xyz coordinates.
#' @param cloud A \code{data.table} of a point cloud with xyz coordinates to extract the neighboring points.
#' @param k An integer of a length 1 representing the number of neighbors to consider.
#' @param radius Optional. A \code{numeric} vector of a length 1 representing a priori radius from \code{x} to select the k nearest neighbors. This speed up the calculations when \code{cloud} is too large.
#' @param basic Logical, if \code{TRUE} it estimate basic metrics. \code{basic = TRUE} as default.
#' @param distribution Logical, if \code{TRUE} it estimate distribution metrics of points. \code{distribution = TRUE} as default.
#' @param dimensionality Logical, if \code{TRUE} it estimate dimensionality metrics. \code{dimensionality = TRUE} as default.
#' @param n_replicates A positive \code{interger} of a length 1 representing the number of replicates to estimate the expected distance. If \code{n_replicates = NULL}, it uses the number of neighboring points calculated. \code{n_replicates = NULL} as default.
#'
#' @return A \code{data.table} with the xyz coordinates of the target point and the computed metrics by \code{basic.metrics()}, \code{distribution()}, and \code{dimensionality()} functions.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @examples
#' data("pc_tree")
#' knn_metrics(pc_tree[100,], pc_tree, k = 10, radius = 0.2)
#'
#' @export
knn_metrics <- function(x, cloud, k, radius = NULL, basic = TRUE, distribution = TRUE, dimensionality = TRUE, n_replicates = NULL) {

  colnames(cloud) <- c("X", "Y", "Z")

  xcoor <- as.numeric(x[1,1]) ###Set coordinates of interest
  ycoor <- as.numeric(x[1,2])
  zcoor <- as.numeric(x[1,3])

  if(is.null(radius) == TRUE) {

    space <- cloud[,1:3]

    space <- space[, distance := sqrt((xcoor - space$X)^2 + (ycoor - space$Y)^2 + (zcoor - space$Z)^2)] #Get the distance of the points
    space <- space[space$distance > 0,]
    space <- setorder(space, distance)

  } else {

    cube <- cloud[between(X, xcoor - radius, xcoor + radius) & between(Y, ycoor - radius, ycoor + radius) & between(Z, zcoor - radius, zcoor + radius),] ###Set a cube to estimate the distance

    cube <- cube[,1:3]
    cube <- cube[,distance := sqrt((xcoor - cube$X)^2 + (ycoor - cube$Y)^2 + (zcoor - cube$Z)^2)] #Get the distance of the points in the cube
    space <- cube[cube$distance <= radius & cube$distance > 0,] #Create the sphere incide the cube
    space <- setorder(space, distance) #Order points by distance
  }

  space <- space[c(1:k),]

  final <- data.table(X = xcoor, Y = ycoor, Z = zcoor)

  if(basic == TRUE) {
    rbasic <- basic_metrics(space, radius)
    final <- cbind(final, rbasic)
  }

  if(distribution == TRUE) {
    rdistribution <- distribution(space, radius, n_replicates)
    final <- cbind(final, rdistribution)
  }

  if(dimensionality == TRUE) {
    rdimensionality <- dimensionality(space)
    final <- cbind(final, rdimensionality)
  }

  return(final)
}
