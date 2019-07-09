#' @title Sphere Metrics on a Target Point
#'
#' @description Estimate different metrics on a neighborhood of a Targed point using a sphere.
#'
#' @param x  A \code{data.table} of the target point with three columns of the *XYZ* coordinates.
#' @param cloud A \code{data.table} of a point cloud with three columns of the *XYZ* coordinates to extract the neighboring points.
#' @param radius A \code{numeric} vector of a length 1 representing a priori radius from \code{x} to select the k-nearest neighbors. This speed up the calculations when \code{cloud} is too large.
#' @param basic Logical, if \code{TRUE} it estimates \code{\link{basic_metrics}}. \code{TRUE} as default.
#' @param distribution Logical, if \code{TRUE} it estimates \code{\link{distribution}} metrics of points. \code{TRUE} as default.
#' @param dimensionality Logical, if \code{TRUE} it estimates \code{\link{dimensionality}} metrics. \code{TRUE} as default.
#' @param n_replicates If \code{distribution = TRUE}, a positive \code{interger} of a length 1 representing the number of replicates to estimate the expected distance. If \code{NULL}, it uses the number of neighboring points calculated. \code{NULL} as default.
#'
#' @seealso \code{\link{basic_metrics}}, \code{\link{distribution}}, \code{\link{dimensionality}}, \code{\link{cloud_metrics}}
#'
#' @return A \code{data.table} with the *XYZ* coordinates of the target point and the computed metrics.
#' @author J. Antonio Guzmán Q.
#'
#' @importFrom data.table :=
#' @importFrom data.table setorder
#'
#' @examples
#' data("pc_tree")
#' sphere_metrics(pc_tree[100,], pc_tree, radius = 0.2)
#'
#' @export
sphere_metrics <- function(x, cloud, radius, basic = TRUE, distribution = TRUE, dimensionality = TRUE, n_replicates = NULL) {

  colnames(cloud) <- c("X", "Y", "Z")

  xcoor <- as.numeric(x[1,1]) ###Set coordinates of interest
  ycoor <- as.numeric(x[1,2])
  zcoor <- as.numeric(x[1,3])

  cube <- cloud[X >= (xcoor - radius) & X <= (xcoor + radius) & ###Set a cube to estimate the distance
                Y >= (ycoor - radius) & Y <= (ycoor + radius) &
                Z >= (zcoor - radius) & Z <= (zcoor + radius)]

  cube <- cube[,1:3]

  cube <- cube[, distance := sqrt((xcoor - cube$X)^2 + (ycoor - cube$Y)^2 + (zcoor - cube$Z)^2)] #Get the distance of the points in the cube

  space <- cube[cube$distance <= radius & cube$distance > 0,] #Create the sphere incide the cube
  space <- setorder(space, distance) #Order points by distance

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
