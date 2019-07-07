#' @title Tree point cloud
#'
#' @description Data from a point cloud of a tree with a spatial point resolution of 0.05 mm.
#'
#' @docType data
#'
#' @usage data(pc_tree)
#'
#' @format A \code{data.table} where the rows represent the points and the three columns represent the *XYZ* coordinates.
#'
#' @keywords datasets
#'
#' @references Guzman, Sharp, Alencastro, Sanchez-Azofeifa. 2018. To be published.
#'
#' @examples
#' data(pc_tree)
#' head(pc_tree)
#' plot3d(pc_tree)
"pc_tree"
