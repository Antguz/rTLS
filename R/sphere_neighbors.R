#' @title Neighboring points in a sphere
#'
#' @description Estimate the neighboring points in a shpere around the targed point.
#'
#' @param x  A \code{data.table} of the targed point with xyz coordinates.
#' @param cloud A \code{data.table} of a point cloud with xyz coordinates to extract the neighboring points.
#' @param radius A numeric \code{vector} of a length 1 representing the minimum distance to consider neighboring points.
#'
#' @return A \code{data.frame} with the xyz coordinates of the neighboring points and a fourth column with their distance.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @examples
#' data("pc_tree")
#' x <- pc_tree[100,]
#' sphere_neighbors(targed = x, cloud = pc_tree, radius = 0.2)
#'
#' @export
sphere_neighbors <- function(targed, cloud, radius) {

  colnames(targed) <- c("X", "Y", "Z")
  colnames(cloud) <- c("X", "Y", "Z")

  xcoor <- targed$X[1] ###Set coordinates of interest
  ycoor <- targed$Y[1]
  zcoor <- targed$Z[1]

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
