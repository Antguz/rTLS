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
#' @details A TLS scan conducted using a Reigel VZ400i with a vertical and
#' horizontal resolution of 0.048 and 0.622 degrees (2082 and 580 lines, respectively).
#' The scanner has frame of scanning between 30 and 130.024 degrees zenith and 0 and
#' 359.90 degrees azimuth. At the moment of the scan the roll, pitch, and yaw of
#' the scanner were 1.026, 0.746, -110.019, respectively. The scanner coordinates
#' in this scan are \code{x = 0, y = 0, z = 0}.
#'
#' @usage data(TLS_scan)
#'
#' @format A \code{data.table} where the rows represent the pulse returns and the
#' three columns represent the *XYZ* coordinates, and the target count and index.
#'
#' @keywords datasets
#'
#' @examples
#' data(TLS_scan)
#' head(TLS_scan)
#'
"TLS_scan"
