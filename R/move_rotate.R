#' @title Moves and Rotates a Point Cloud
#'
#' @description Moves a point cloud based on *XYZ* coordinates, and rotates it based on the roll, pitch, and yaw angles.
#'
#' @param cloud A \code{data.table} with three columns describing the *XYZ* coordinates of a point cloud.
#' @param move A \code{numeric} vector of length three describing the *XYZ* coordinates to move \code{cloud}.
#' @param rotate A \code{numeric} vector of length three describing the rotation angles (degrees) for the roll, pitch, and yaw.
#'
#' @details \code{move} conducts a substraction between \code{cloud} less \code{move} coordinates. It assumes to be \code{move = c(X = 0, Y = 0, Z = 0)}.
#' Likewise, \code{rotate} assumes that roll has an effect on the *X* axis, the pitch on the *Y* axis, and the yaw on the *Z* axis. The rotation is based on
#' E-N-U coordinates (ENU system, East-North-Up). It assumes to be \code{move = c(roll = 0, pitch = 0, yaw = 0)}.
#'
#' @return A \code{data.table} with the rotation and move applied to \code{cloud}.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @examples
#'
#' data(pc_tree)
#' rgl::plot3d(pc_tree)
#' coordinates <- c(mean(pc_tree$X), mean(pc_tree$Y), mean(pc_tree$Z))
#' degrees <- c(45, 45, 0)
#' rgl::plot3d(move_rotate(pc_tree, coordinates, degrees))
#'
#'
#' @export
move_rotate <- function(cloud, move = c(0, 0, 0), rotate = c(0, 0, 0)) {

  colnames(cloud)[1:3] <- c("X", "Y", "Z")

  ####Move the point cloud---------------------------------------

  if((class(move) != "numeric") & (length(move) != 3)) {
      stop("move needs to be a numeric vector of length 3")
  }

  if((class(move) != "rotate") & (length(rotate) != 3)) {
    stop("rotate needs to be a numeric vector of length 3")
  }

  ####Rotates the point cloud ------------------------------------------------------------------------

  new_cloud <- move_rotate_rcpp(as.matrix(cloud), move, rotate)

  new_cloud <- as.data.table(new_cloud)
  colnames(new_cloud) <- c("X", "Y", "Z")

  return(new_cloud)

}
