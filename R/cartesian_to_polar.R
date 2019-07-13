#' @title Cartesian to Polar Coordinates
#'
#' @description Conver from cartesian to polar coordinates.
#'
#' @param cartesian A \code{data.table} with three columns describing the *XYZ* coordinates of a point cloud.
#' @param anchor A \code{numeric} vector of length three which describe the *XYZ* anchor coordinate for reference to get the polar coordinates.
#' If \code{NULL}, it assumes that the reference coordinate is \code{c(X = 0, Y = 0, Z = 0)}.
#'
#' @details It assumes that the positive *Z* axis is the reference vector for the zenith angle. Likewise, it assumes that the *Y* axis is the north-south direction (positive to negative) for the azimuth angle.
#' If a point from \code{cartesian} presents the same *XY* coordinates than \code{anchor}, \code{angles} returns \code{NA}.
#'
#' @return A \code{data.table} with the zenith and azimuth angles (degrees), and the distance to the anchor coordinate.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @examples
#'
#' data(pc_tree)
#' cartesian_to_polar(pc_tree)
#' anchor <- c(1, 1, 1)
#' cartesian_to_polar(pc_tree, anchor)
#'
#' @export
cartesian_to_polar <- function(cartesian, anchor = NULL) {

  if(is.null(anchor) == TRUE) {
    anchor <- c(X = 0, Y = 0, Z = 0)
  } else if(class(anchor) != "numeric" | length(anchor) != 3) {
    stop("Anchor needs to be a numeric vector of length 3 representing XYZ")
  }

  colnames(cartesian)[1:3] <- c("X", "Y", "Z")

  cartesian <- cartesian[, distance := sqrt((anchor[1] - X)^2 + (anchor[2] - Y)^2 + (anchor[3] - Z)^2)] #Estimates distance

  ####Zenith angle estimation --------------------------------------------------------------------------------------------------------------
  cartesian <- cartesian[Z > anchor[3], zenith := (180 * acos((Z - anchor[3])/distance))/pi] ###Estimates the north zenith angle hemisphere
  cartesian <- cartesian[Z == anchor[3], zenith := 90]
  cartesian <- cartesian[Z < anchor[3], zenith := 90 + (180 * acos((anchor[3] - Z)/distance))/pi] ###Estimates the south zenith angle hemisphere

  ####Azimuth angle estimation -------------------------------------------------------------------------------------------------------------
  cartesian <- cartesian[X > anchor[1] & Y > anchor[2], azimuth := (180 * atan((X-anchor[1])/(Y-anchor[2])))/pi] ###Quadrant 1
  cartesian <- cartesian[X > anchor[1] & Y < anchor[2], azimuth := 180 - ((180 * atan((X-anchor[1])/(anchor[2]-Y)))/pi)] ###Quadrant 2
  cartesian <- cartesian[X < anchor[1] & Y < anchor[2], azimuth := 180 + ((180 * atan((anchor[1]-X)/(anchor[2]-Y)))/pi)] ###Quadrant 3
  cartesian <- cartesian[X < anchor[1] & Y > anchor[2], azimuth := 360 - ((180 * atan((anchor[1]-X)/(Y-anchor[2])))/pi)] ###Quadrant 4

  cartesian <- cartesian[X == anchor[1] & Y > anchor[2], azimuth := 0] ###Zenith angle
  cartesian <- cartesian[X > anchor[1] & Y == anchor[2], azimuth := 90]
  cartesian <- cartesian[X == anchor[1] & Y < anchor[2], azimuth := 180]
  cartesian <- cartesian[X < anchor[1] & Y == anchor[2], azimuth := 270]
  cartesian <- cartesian[X == anchor[1] & Y == anchor[2], azimuth := NA]

  return(cartesian[, c(5,6,4)])
}
