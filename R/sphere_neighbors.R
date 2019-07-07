#' @title Neighboring Points in a Sphere
#'
#' @description Estimate the neighboring points in a sphere around the target point.
#'
#' @param x  A \code{data.table} of the target point with three columns of the *XYZ* coordinates.
#' @param cloud A \code{data.table} of a point cloud with three columns of the *XYZ* coordinates to extract the neighboring points.
#' @param radius A numeric \code{vector} of a length 1 representing the minimum distance to consider neighboring points.
#'
#' @return A \code{data.frame} with the three columns of the *XYZ* coordinates of the neighboring points and a fourth column with their distance.
#' @details If a neighboring point presents a distance equal to zero, this is automatically removed. This is conducted to avoid \code{x} points that are already embedded in \code{cloud}.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @seealso \code{\link{knn_neighbors}}, \code{\link{neighborhood}}
#'
#' @examples
#' data("pc_tree")
#' x <- pc_tree[100,]
#' sphere_neighbors(x = x, cloud = pc_tree, radius = 0.2)
#'
#'
#' @export
sphere_neighbors <- function(x, cloud, radius) {

  colnames(x) <- c("X", "Y", "Z")
  colnames(cloud) <- c("X", "Y", "Z")

  xcoor <- x$X[1] ###Set coordinates of interest
  ycoor <- x$Y[1]
  zcoor <- x$Z[1]

  cube <- cloud[X >= (xcoor - radius) & X <= (xcoor + radius) & ###Set a cube to estimate the distance
                Y >= (ycoor - radius) & Y <= (ycoor + radius) &
                Z >= (zcoor - radius) & Z <= (zcoor + radius)]

  cube <- cube[,distance := sqrt((xcoor - X)^2 + (ycoor - Y)^2 + (zcoor - Z)^2)] #Get the distance of the points in the cube

  space <- cube[distance <= radius & distance > 0] #Create the sphere incide the cube

  if(nrow(space) < 1) {
    return(space[1,])

  } else {
    space <- setorder(space, distance) #Order points by distance
    return(space)
  }
}
