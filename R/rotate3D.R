#' @title Rotate a Point Cloud
#'
#' @description Rotate point cloud based on the roll, pitch, and yaw angles.
#'
#' @param cloud A \code{data.table} with three columns describing the *XYZ* coordinates of a point cloud.
#' @param roll A \code{numeric} vector describing the degrees of rotation angles for roll (*X*).
#' @param pitch A \code{numeric} vector describing the degrees of rotation angles for pitch (*Y*).
#' @param yaw A \code{numeric} vector describing the degrees of rotation angles for yaw (*Z*). for the roll, pitch, and yaw.
#' @param threads An \code{integer} specifying the number of threads to use. Experiment to see what works best for your data on your hardware.
#'
#' @details The *XYZ* coordinates are transformed to E-N-U coordinates (ENU system, East-North-Up).
#'
#' @return A \code{data.table} with the rotation applied to \code{cloud}.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @importFrom data.table as.data.table
#'
#' @examples
#'
#' data(pc_tree)
#' rgl::plot3d(pc_tree)
#' rgl::plot3d(rotate3D(pc_tree, roll = 45, pitch = 45, yaw = 0))
#'
#' @export
rotate3D <- function(cloud, roll = 0, pitch = 0, yaw = 0, threads = 1) {

  ####Rotates the point cloud ------------------------------------------------------------------------

  new_cloud <- rotate3D_rcpp(as.matrix(cloud), roll, pitch, yaw, threads)

  new_cloud <- as.data.table(new_cloud)
  colnames(new_cloud) <- c("X", "Y", "Z")

  return(new_cloud)

}
