#' @title Cartesian to Polar Coordinates
#'
#' @description Convert from East-North-Up cartesian coordinates to polar coordinates.
#'
#' @param cartesian A \code{data.table} with three columns describing the *XYZ* coordinates of a point cloud.
#' @param anchor A \code{numeric} vector of length three which describe the *XYZ* anchor coordinate for reference to get the polar coordinates. It assumes that the reference coordinates are \code{c(X = 0, Y = 0, Z = 0)} as default.
#' @param digits A \code{numeric} vector of length 1 describing the decimal numbers to \code{\link{round}} the zenith and azimuth angles. If \code{NULL}, \code{\link{round}} does not apply. \code{NULL} as default.
#'
#'
#' @details It assumes that the positive *Z* axis is the reference vector for the zenith angle. Likewise, it assumes that the *Y* axis is the north-south direction (positive to negative) for the azimuth angle.
#' If a point from \code{cartesian} presents the same *XY* coordinates than \code{anchor}, \code{angles} returns \code{NA}.
#'
#' @return A \code{data.table} with the zenith and azimuth angles (degrees), and the distance to the anchor coordinate.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @useDynLib rTLS, .registration = TRUE
#'
#' @seealso \code{\link{polar_to_cartesian}}
#'
#' @examples
#'
#' data(pc_tree)
#' cartesian_to_polar(pc_tree)
#' anchor <- c(1, 1, 1)
#' cartesian_to_polar(pc_tree, anchor)
#'
#' @export
cartesian_to_polar <- function(cartesian, anchor = c(0 , 0, 0), digits = NULL) {

  if(class(anchor) != "numeric" | length(anchor) != 3) {
    stop("Anchor needs to be a numeric vector of length 3 representing X, Y, and Z")
  }

  polar <- cartesian_to_polar_rcpp(as.matrix(cartesian), anchor)
  polar <- as.data.table(polar)
  colnames(polar) <- c("zenith", "azimuth", "distance")

  polar[, azimuth := ((azimuth*pi/360) %% pi)*360/pi, by = seq_along(1:nrow(polar))]


  if(is.null(digits) != TRUE) {
    polar[, c("zenith", "azimuth", "distance") := round(.SD, digits), .SDcols= c("zenith", "azimuth", "distance")]
  }

  return(polar)
}
