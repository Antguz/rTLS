#' @title K-nearest Neighbors Metrics on a Target Point
#'
#' @description Estimate different metrics on a neighborhood of a target point using knn.
#'
#' @param x  A \code{data.table} of the target point with three columns of the *XYZ* coordinates.
#' @param cloud A \code{data.table} of a point cloud with three columns of the *XYZ* coordinates to extract the neighboring points.
#' @param k An \code{integer} of a length 1 representing the number of neighbors to consider.
#' @param radius Optional. A \code{numeric} vector of a length 1 representing a priori radius from \code{x} to select the k-nearest neighbors. This speed up the calculations when \code{cloud} is too large.
#' @param basic Logical, if \code{TRUE} it estimates \code{\link{basic_metrics}}. \code{TRUE} as default.
#' @param distribution Logical, if \code{TRUE} it estimates \code{\link{distribution}} metrics of points. \code{TRUE} as default.
#' @param dimensionality Logical, if \code{TRUE} it estimates \code{\link{dimensionality}} metrics. \code{TRUE} as default.
#' @param n_replicates If \code{distribution = TRUE}, a positive \code{interger} of a length 1 representing the number of replicates to estimate the expected distance. If \code{n_replicates = NULL}, it uses the number of neighboring points calculated. \code{NULL} as default.
#'
#' @seealso \code{\link{basic_metrics}}, \code{\link{distribution}}, \code{\link{dimensionality}}, \code{\link{cloud_metrics}}
#'
#' @return A \code{data.table} with the *XYZ* coordinates of the target point and the computed metrics.
#' @author J. Antonio Guzmán Q.
#'
#' @importFrom stats na.exclude
#' @importFrom data.table :=
#' @importFrom data.table setorder
#'
#' @examples
#' data("pc_tree")
#' knn_metrics(pc_tree[100,], pc_tree, k = 10)
#'
#' @export
knn_metrics <- function(x, cloud, k, radius = NULL, basic = TRUE, distribution = TRUE, dimensionality = TRUE, n_replicates = NULL) {

  colnames(cloud) <- c("X", "Y", "Z")

  xcoor <- as.numeric(x[1,1]) ###Set coordinates of interest
  ycoor <- as.numeric(x[1,2])
  zcoor <- as.numeric(x[1,3])

  if(is.null(radius) == TRUE) {

    space <- cloud[,1:3]

    space <- space[, distance := distanceC(xcoor, ycoor, zcoor, space$X, space$Y, space$Z)] #Get the distance of the points
    space <- space[space$distance > 0,]
    space <- setorder(space, distance)

  } else {

    cube <- cloud[X >= (xcoor - radius) & X <= (xcoor + radius) & ###Set a cube to estimate the distance
                  Y >= (ycoor - radius) & Y <= (ycoor + radius) &
                  Z >= (zcoor - radius) & Z <= (zcoor + radius)]

    cube <- cube[,1:3]
    cube <- cube[, distance := distanceC(xcoor, ycoor, zcoor, cube$X, cube$Y, cube$Z)] #Get the distance of the points in the cube
    space <- cube[cube$distance > 0,] #Create the sphere incide the cube
    space <- setorder(space, distance) #Order points by distance
  }

  space <- na.exclude(space[c(1:k),])

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
