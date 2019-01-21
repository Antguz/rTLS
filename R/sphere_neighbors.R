#' @import dplyr
#'
#' @title Neighboring points in a sphere
#'
#' @description Estimate the neighboring points in a shpere around the targed point.
#'
#' @param x  A \code{matrix} or \code{data.frame} of the targed point with xyz coordinates.
#' @param cloud A \code{matrix} or \code{data.frame} of a point cloud with xyz coordinates to extract the neighboring points.
#' @param radius A numeric \code{vector} of a length 1 representing the minimum distance to consider neighboring points.
#'
#' @return A \code{data.frame} with the xyz coordinates of the neighboring points and a fourth column with their distance.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @examples
#' data("pc_tree")
#' sphere_neighbors(pc_tree[100,], pc_tree, radius = 0.5)
#'
#' @export
sphere_neighbors <- function(x, cloud, radius) {

  xcoor <- as.numeric(x[1,1])
  ycoor <- as.numeric(x[1,2])
  zcoor <- as.numeric(x[1,3])

  cube <- cloud %>% filter(cloud[,1] <= (xcoor + radius), cloud[,1] >= (xcoor - radius),
                           cloud[,2] <= (ycoor + radius), cloud[,2] >= (ycoor - radius),
                           cloud[,3] <= (zcoor + radius), cloud[,3] >= (zcoor - radius))

  cube <- cube[,1:3]

  cube <- cube %>% mutate(distance = sqrt((xcoor - cube$X)^2 + (ycoor - cube$Y)^2 + (zcoor - cube$Z)^2))

  space <- cube[cube$distance <= radius & cube$distance > 0,]

  space <- arrange(space, distance)
  return(space)
}
