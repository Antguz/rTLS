#' @title Neighboring points in a sphere
#'
#' @description Estimate the neighboring points in a shpere around the targed point.
#'
#' @param x  A \code{data.frame} or \code{data.table} of the targed point with xyz coordinates.
#' @param cloud A \code{data.table} of a point cloud with xyz coordinates to extract the neighboring points.
#' @param radius A numeric \code{vector} of a length 1 representing the minimum distance to consider neighboring points.
#'
#' @return A \code{data.frame} with the xyz coordinates of the neighboring points and a fourth column with their distance.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @examples
#' data("pc_tree")
#' sphere_neighbors(pc_tree[100,], pc_tree, radius = 0.2)
#'
#' @export
sphere_neighbors <- function(x, cloud, radius) {

  colnames(cloud) <- c("X", "Y", "Z")

  xcoor <- as.numeric(x[1,1]) ###Set coordinates of interest
  ycoor <- as.numeric(x[1,2])
  zcoor <- as.numeric(x[1,3])

  cube <- cloud[between(X, xcoor - radius, xcoor + radius) & between(Y, ycoor - radius, ycoor + radius) & between(Z, zcoor - radius, zcoor + radius),] ###Set a cube to estimate the distance

  cube <- cube[,1:3]

  cube <- cube[,distance := sqrt((xcoor - cube$X)^2 + (ycoor - cube$Y)^2 + (zcoor - cube$Z)^2)] #Get the distance of the points in the cube

  space <- cube[cube$distance <= radius & cube$distance > 0,] #Create the sphere incide the cube

  if(nrow(space) < 1) {
    return(space[1,])

  } else {
    space <- setorder(space, distance) #Order points by distance
    return(space)
  }
}
