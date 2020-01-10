#' @title Moves and Rotates a Point Cloud
#'
#' @description Moves a point cloud based on *XYZ* coordinates, and rotates it based on the roll, pitch, and yaw angles.
#'
#' @param cloud A \code{data.table} with three columns describing the *XYZ* coordinates of a point cloud.
#' @param move A \code{numeric} vector of length three describing the *XYZ* coordinates to move \code{cloud}.
#' @param rotate A \code{numeric} vector of length three describing the rotation angles (degrees) for the roll, pitch, and yaw.
#'
#' @details \code{move} conducts a substraction between \code{cloud} less \code{move} coordinates. If \code{NULL}, it does not apply \code{move}.
#' Likewise, \code{rotate} assumes that roll has an effect on the *X* axis, the pitch on the *Y* axis, and the yaw on the *Z* axis. The rotation is based on
#' E-N-U coordinates (ENU system, East-North-Up). If \code{NULL}, it does not apply \code{rotate}.
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
move_rotate <- function(cloud, move, rotate) {

  colnames(cloud)[1:3] <- c("X", "Y", "Z")

  ####Move the point cloud---------------------------------------

  if(is.null(move) == TRUE) {
    cloud <- cloud

  } else {

    if(class(move) != "numeric") {
      stop("Move needs to be a numeric vector")
    }

    if(length(move) != 3) {
      stop("Move needs to be a numeric vector of length 3")
    }

    cloud[, 1] <- cloud[,1] - move[1]
    cloud[, 2] <- cloud[,2] - move[2]
    cloud[, 3] <- cloud[,3] - move[3]
  }

  ####Rotates the point cloud ------------------------------------------------------------------------


  if(is.null(rotate) == TRUE) {
    cloud <- cloud

  } else {

    if(class(rotate) != "numeric") {
      stop("Rotate needs to be a numeric vector")
    }

    if(length(rotate) != 3) {
      stop("Rotate needs to be a numeric vector of length 3")
    }


    rotate <- rotate*pi/180 #convert degrees to radiants

    if(rotate[3] != 0) {
      cloud[, c("X", "Y") := list(((X * cos(rotate[3])) - (Y * sin(rotate[3]))),
                                 ((X * sin(rotate[3])) + (Y * cos(rotate[3])))),
                                          by = seq_len(nrow(cloud))]
    }

    if(rotate[2] != 0) {
      cloud[, c("X", "Z") := list(((X * cos(rotate[2])) - (Z * sin(rotate[2]))),
                                  ((X * sin(rotate[2])) + (Z * cos(rotate[2])))),
                                    by = seq_len(nrow(cloud))]
    }

    if(rotate[1] != 0) {
      cloud[, c("Y", "Z") := list(((Y * cos(rotate[1])) - (Z * sin(rotate[1]))),
                                  ((Y * sin(rotate[1])) + (Z * cos(rotate[1])))),
                                    by = seq_len(nrow(cloud))]
    }
  }

  return(cloud)
}
