#' @title Moves and Rotates a Point Cloud
#'
#' @description Moves a point cloud based on *XYZ* coordinates, and rotates it based on the pitch, roll, and yaw angles.
#'
#' @param cloud A \code{data.table} with three columns describing the *XYZ* coordinates of a point cloud.
#' @param move A \code{numeric} vector of length three describing the *XYZ* coordinates to move \code{cloud}.
#' @param rotate A \code{numeric} vector of length three describing the rotation angles (degrees) for the pitch, roll, and yaw.
#'
#' @details \code{move} conducts a substraction between \code{cloud} less \code{move} coordinates. If \code{NULL}, it does not apply \code{move}.
#' Likewise, \code{rotate} assumes that pitch has an effect on the *X* axis, the roll on the *Y* axis, and the yaw on the *Z* axis. If \code{NULL}, it does not apply \code{rotate}.
#'
#' @return A \code{data.table} with the rotation and move applied to \code{cloud}.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @examples
#'
#' data(pc_tree)
#' coordinates <- c(mean(pc_tree$X), mean(pc_tree$Y), mean(pc_tree$Z))
#' degrees <- c(0, 0, 180)
#' move_rotate(pc_tree, coordinates, degrees)
#'
#' @export
move_rotate <- function(cloud, move, rotate) {

  ####Move the point cloud---------------------------------------

  if(is.null(move) == TRUE) {
    cloud <- cloud

  } else if(class(move) != "numeric") {
    stop("Move needs to be a numeric vector")

  } else if(length(move) != 3) {
    stop("Move needs to be a numeric vector of length 3")

  } else {
    cloud[, 1] <- cloud[,1] - move[1]
    cloud[, 2] <- cloud[,2] - move[2]
    cloud[, 3] <- cloud[,3] - move[3]
  }

  ####Rotates the point cloud ------------------------------------------------------------------------

  if(is.null(rotate) == TRUE) {
    cloud <- cloud

  } else if(class(rotate) != "numeric") {
    stop("Rotate needs to be a numeric vector")

  } else if(length(rotate) != 3) {
    stop("Rotate needs to be a numeric vector of length 3")

  } else {
    rotate <- rotate*pi/180 #convert degrees to radiants

    cosa <- cos(rotate[3])
    sina <- sin(rotate[3])

    cosb <- cos(rotate[1])
    sinb <- sin(rotate[1])

    cosc <- cos(rotate[2])
    sinc <- sin(rotate[2])

    Axx <- cosa*cosb
    Axy <- cosa*sinb*sinc - sina*cosc
    Axz <- cosa*sinb*cosc + sina*sinc

    Ayx <- sina*cosb
    Ayy <- sina*sinb*sinc + cosa*cosc
    Ayz <- sina*sinb*cosc - cosa*sinc

    Azx <- -sinb
    Azy <- cosb*sinc
    Azz <- cosb*cosc

    cloud <- cloud[, c("X", "Y", "Z") := list((Axx*X + Axy*Y + Axz*Z),
                                              (Ayx*X + Ayy*Y + Ayz*Z),
                                              (Azx*X + Azy*Y + Azz*Z)), by = seq_len(nrow(cloud))]
  }
  return(cloud)
}
