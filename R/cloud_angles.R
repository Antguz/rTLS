#' @title Zenith and Azimuth of a Point Cloud
#'
#' @description Estimates the zenith and azimuth angle of a point cloud based on a reference coordinate.
#'
#' @param cloud A \code{data.table} with three columns describing the *XYZ* coordinates of a point cloud.
#' @param coordinates A \code{numeric} vector of length three which describe the *XYZ* reference coordinate.
#' If \code{NULL}, it assumes that the reference coordinate is \code{c(X = 0, Y = 0, Z = 0)}.
#'
#' @details It assumes that the positive *Z* axis is the reference vector for the zenith angle. Likewise, it assumes that the *Y* axis is the north-south direction (positive to negative) for the azimuth angle.
#' If a point from \code{cloud} presents the same *XY* coordinates than \code{coordinates}, \code{angles} returns \code{NA}.
#'
#' @return A \code{data.table} with the zenith and azimuth angles per point in degrees.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @examples
#'
#' data(pc_tree)
#' coordinates <- c(mean(pc_tree$X), mean(pc_tree$Y), 0)
#' angles(pc_tree, coordinates)
#'
#' @export
angles <- function(cloud, coordinates) {

  colnames(cloud[,1:3]) <- c("X", "Y", "Z")

  cloud <- cloud[, distance := sqrt((coordinates[1] - X)^2 + (coordinates[2] - Y)^2 + (coordinates[3] - Z)^2)] #Estimates distance

  ####Zenith angle estimation --------------------------------------------------------------------------------------------------------------
  cloud <- cloud[Z > coordinates[3], zenith := (180 * acos((Z - coordinates[3])/distance))/pi] ###Estimates the north zenith angle hemisphere
  cloud <- cloud[Z == coordinates[3], zenith := 90]
  cloud <- cloud[Z < coordinates[3], zenith := 90 + (180 * acos((coordinates[3] - Z)/distance))/pi] ###Estimates the south zenith angle hemisphere

  ####Azimuth angle estimation -------------------------------------------------------------------------------------------------------------
  cloud <- cloud[X > coordinates[1] & Y > coordinates[2], azimuth := (180 * atan((X-coordinates[1])/(Y-coordinates[2])))/pi] ###Quadrant 1
  cloud <- cloud[X > coordinates[1] & Y < coordinates[2], azimuth := 180 - ((180 * atan((X-coordinates[1])/(coordinates[2]-Y)))/pi)] ###Quadrant 2
  cloud <- cloud[X < coordinates[1] & Y < coordinates[2], azimuth := 180 + ((180 * atan((coordinates[1]-X)/(coordinates[2]-Y)))/pi)] ###Quadrant 3
  cloud <- cloud[X < coordinates[1] & Y > coordinates[2], azimuth := 360 - ((180 * atan((coordinates[1]-X)/(Y-coordinates[2])))/pi)] ###Quadrant 4

  cloud <- cloud[X == coordinates[1] & Y > coordinates[2], azimuth := 0] ###Zenith angle
  cloud <- cloud[X > coordinates[1] & Y == coordinates[2], azimuth := 90]
  cloud <- cloud[X == coordinates[1] & Y < coordinates[2], azimuth := 180]
  cloud <- cloud[X < coordinates[1] & Y == coordinates[2], azimuth := 270]
  cloud <- cloud[X == coordinates[1] & Y == coordinates[2], azimuth := NA]

  return(cloud[, (ncol(cloud)-1):ncol(cloud)])
}
