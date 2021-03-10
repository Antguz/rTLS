#' @title Tree Metrics
#'
#' @description Estimate the tree height, crown area, and the diameter at breast height of a tree point cloud
#'
#' @param cloud A \code{data.table} of the target point with three columns of the *XYZ* coordinates.
#' @param region.diameter A \code{numeric} vector of length 2 indicating the lower and higher region to subset the point cloud and get the diameter. If \code{region.diameter = NULL}, it use \code{c(1.25, 1.35)}. \code{NULL} as default.
#' @param relocateZ Logical, if \code{TRUE} it relocates the *Z* coordinates to a minimum coordinate of zero based on the current \code{min(cloud[,3])}. Useful if the base value (*Z*) of a tree point cloud is not topography corrected.
#'
#' @return A \code{data.table} with the tree height, crown area, and diameter
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @details The tree height is estimated based on the maximum value of *Z*, the
#' crown area is calculated applying a convex hull on the point cloud, while the
#' DBH is calculated extracting the area of the convex hull on the subset of points
#' between \code{region.diameter}, and then estimating the diameter of a circle.
#' For another estimation of DBH try \code{\link{circleRANSAC}} or for irregular
#' trucks try \code{\link{trunk_volume}}.
#'
#'
#' @importFrom sp Polygon
#' @importFrom data.table data.table
#'
#' @seealso \code{\link{circleRANSAC}}, \code{\link{trunk_volume}}
#'
#' @examples
#' data("pc_tree")
#' tree_metrics(pc_tree)
#'
#' @export
tree_metrics <- function(cloud, region.diameter = NULL, relocateZ = TRUE) {

  colnames(cloud[,1:3]) <- c("X", "Y", "Z")

  if(relocateZ == TRUE) {
    cloud[,3] <- cloud[,3] - min(cloud[,3])
  }

  Height <- max(cloud[,3]) ### Tree height

  ch <- chull(cloud[,1:2]) ###Crown area
  extreme_coor <- cloud[ch, ]
  poly_crown <- Polygon(extreme_coor[,1:2], hole=F)
  Crown_area <- poly_crown@area

  if(is.null(region.diameter) == TRUE) {
    region.diameter <- c(min(cloud[,3]) + 1.25, min(cloud[,3]) + 1.35)
  }

  sub <- cloud[between(Z, region.diameter[1], region.diameter[2]),]
  sub <- sub[order(Z),]
  ch <- chull(sub[,1:2])
  extreme_coor <- sub[c(ch, ch[1]), 1:2]
  poly_DBH <- Polygon(extreme_coor, hole=F)
  DBH <- sqrt(poly_DBH@area/pi)*2

  frame <- data.table(Height, Crown_area, DBH)

  return(frame)
}

