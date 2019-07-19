#' @title A TLS scan
#'
#' @description A \code{data.table} from a TLS scan.
#'
#' @docType data
#' @format A \code{data.table} with five columns, which are:
#' \describe{
#' \item{X}{the "X" coordinate}
#' \item{Y}{the "Y coordinate}
#' \item{Z}{the "Z" coordinate}
#' \item{Target_count}{The number of received by the same laser shot}
#' \item{Target_index}{The rank of the returned pulse in the target count of received by the same laser shot}
#' }
#'
#' @details A TLS scan conducted using a Reigel VZ400i with a vertical and horizonal resolution of 0.04 degrees. The scanner has frame of scanning between 30 and 130 degrees zenith and 0 and 360 degrees azimuth.
#' At the moment of the scan the roll, pitch, and yaw of the scanner were 0.293, -0.835, -150.159, respectivly. The scanner coodinates in this scan are \code{x = 0, y = 0, z = 0}.
#'
#' @usage data(TLS_scan)
#'
#' @format A \code{data.table} where the rows represent the pulse retunrs and the three columns represent the *XYZ* coordinates, and the target count and index.
#'
#' @keywords datasets
#'
#' @importFrom rgl plot3d
#'
#' @examples
#' data(TLS_scan)
#' head(TLS_scan)
#'
"TLS_scan"
