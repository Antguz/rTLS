#' @title Rotate a plane of coordinates
#'
#' @description Rotate a plane of coordinates to a given angle.
#'
#' @param plane A \code{data.table} with two columns describing the plane of coordinates.
#' @param angle A \code{numeric} vector describing the degrees of rotation.
#' @param threads An \code{integer} specifying the number of threads to use. Experiment to see what works best for your data on your hardware.
#'
#' @return A \code{data.table} with the rotation applied to \code{plane}.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @importFrom data.table as.data.table
#'
#' @examples
#'
#' data(pc_tree)
#'
#' plot(pc_tree[,1:2])
#'
#' #Rotate in 45 degrees using Z axis of the cloud
#' plot(rotate2D(pc_tree[,1:2], angle = 45))
#'
#' @export
rotate2D <- function(plane, angle, threads = 1) {

  ####Rotates the point cloud ------------------------------------------------------------------------
  name_plane <- colnames(plane)
  new_cloud <- rotate2D_rcpp(as.matrix(plane), angle, threads)

  new_cloud <- as.data.table(new_cloud)
  colnames(new_cloud) <- name_plane

  return(new_cloud)

}
