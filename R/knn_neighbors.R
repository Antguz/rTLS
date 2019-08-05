#' @title Neighboring Points Based on Knn
#'
#' @description Estimate the neighboring points of a target point based on knn.
#'
#' @param x A \code{data.table} of the target point with three columns of the *XYZ* coordinates.
#' @param cloud A \code{data.table} of with three columns of the *XYZ* coordinates to extract the neighboring points.
#' @param k An \code{integer} of a length 1 representing the number of neighbors to consider.
#' @param radius Optional. A \code{numeric} vector of a length 1 representing a-priori radius from \code{x} to select the k nearest neighbors. This option speed-up the search of neighboring points in highly dense point cloud, but it should be used with caution since small radios may exclude neighboring points.
#'
#' @return A \code{data.frame} with the three columns of the *XYZ* coordinates of the neighboring points and a fourth column with their distance.
#' @author J. Antonio Guzmán Q.
#'
#' @details If a neighboring point presents a distance equal to zero, this is automatically removed. This is conducted to avoid \code{x} points that are already embedded in \code{cloud}.
#'
#' @seealso \code{\link{sphere_neighbors}}, \code{\link{neighborhood}}
#'
#' @importFrom stats na.exclude
#' @importFrom data.table :=
#' @importFrom data.table setorder
#' @importFrom Rcpp sourceCpp
#' @useDynLib rTLS
#'
#' @examples
#' data("pc_tree")
#' knn_neighbors(x = pc_tree[100,], cloud = pc_tree, k = 10)
#' knn_neighbors(x = pc_tree[100,], cloud = pc_tree, k = 10, radius = 0.5)
#'
#'@export
knn_neighbors <- function(x, cloud, k, radius = NULL) {

  colnames(x) <- c("X", "Y", "Z")
  colnames(cloud) <- c("X", "Y", "Z")

  xcoor <- x$X[1] ###Set coordinates of interest
  ycoor <- x$Y[1]
  zcoor <- x$Z[1]

  if(is.null(radius) == TRUE) {

    space <- cloud[,1:3]

    space <- space[, distance := distanceC(xcoor, ycoor, zcoor, space$X, space$Y, space$Z)] #Get the distance of the points
    space <- space[space$distance > 0,]
    space <- setorder(space, distance)

  } else if(is.null(radius) == FALSE){

    cube <- cloud[,1:3]

    cube <- cloud[X >= (xcoor - radius) & X <= (xcoor + radius) & ###Set a cube to estimate the distance
                    Y >= (ycoor - radius) & Y <= (ycoor + radius) &
                    Z >= (zcoor - radius) & Z <= (zcoor + radius)]

    cube <- cube[, distance := distanceC(xcoor, ycoor, zcoor, cube$X, cube$Y, cube$Z)] #Get the distance of the points in the cube
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
