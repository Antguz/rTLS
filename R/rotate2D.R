#' @title Rotate a plane of coordinates
#'
#' @description Rotate a plane of coordinates to a given angle.
#'
#' @param plane A \code{data.table} with two columns describing the plane of coordinates.
#' @param angle A \code{numeric} vector describing the degrees of rotation.
#'
#' @return A \code{data.table} with the rotation applied to \code{plane}.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @examples
#'
#' data(pc_tree)
#' plot(pc_tree[,1:2])
#' plot(rotate2D(pc_tree[,1:2], angle = 45))
#'
#' @export
rotate2D <- function(plane, angle) {

  ####Rotates the point cloud ------------------------------------------------------------------------
  name_plane <- colnames(plane)
  new_cloud <- rotate2D_rcpp(as.matrix(plane), angle)

  new_cloud <- as.data.table(new_cloud)
  colnames(new_cloud) <- name_plane

  return(new_cloud)

}
