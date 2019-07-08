#' @title A Tree Point Cloud
#'
#' @description A \code{data.table} from a point cloud of a tree with a spatial point resolution of 0.05 mm.
#'
#' @docType data
#' @format A \code{data.table} with three columns, which are:
#' \describe{
#' \item{X}{the "X" coordinate}
#' \item{Y}{the "Y coordinate}
#' \item{Z}{the "Z" coordinate}
#' }
#'
#' @usage data(pc_tree)
#'
#' @format A \code{data.table} where the rows represent the points and the three columns represent the *XYZ* coordinates.
#'
#' @keywords datasets
#'
#' @references Guzman, Sharp, Alencastro, Sanchez-Azofeifa. 2018. To be published.
#'
#' @importFrom rgl plot3d
#'
#' @examples
#' data(pc_tree)
#' head(pc_tree)
#'
"pc_tree"
