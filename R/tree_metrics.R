#' @title Tree metrics
#'
#' @description Estimate the tree height, crown area, and diameter at breast height
#'
#' @param cloud A \code{data.table} with xyz coordinates in the first three columns.
#' @param region.diameter A numeric \code{vector} of length 2 indicating the lower and higher region to subset the point cloud and get the diameter. If \code{cloud_b} with xyz coordinates in the first three columns. If \code{region.diameter = NULL}, it use \code{c(1.25, 1.35)}. \code{NULL} as default.
#'
#' @return A \code{data.table} with the tree hight, crown area, and diameter
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @seealso cloud_metrics
#'
#' @examples
#' data("pc_tree")
#' tree_metrics(pc_tree)
#'
#'@export
tree_metrics <- function(cloud, region.diameter = NULL) {

  colnames(cloud[,1:3]) <- c("X", "Y", "Z")

  Height <- max(cloud[,3]) - min(cloud[,3]) ### Tree height

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

