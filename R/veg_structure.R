#' @title Vegetation structure
#'
#' @description Estimates the gap fraction and leaf area density of the vertical profile of a point cloud using the Bouvier et al. 2015 method.
#'
#' @param voxels A \code{"voxels"} object, created with \code{voxels()}.
#' @param k A \code{numeric} vector of length 1 representing the coefficient of extinction. If \code{NULL}, 0.5 is assumed as a proxy to characterize the vegetation properties.
#' @param grid \code{Logic}. If \code{TRUE}
#'
#' @return It return a \code{data.table} wich contain the vertical layer (Z), the number of points at each layer, the gap fraction (GF), and the Leaf Area Density (LAD).
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#' @references Bouvier, M., Durrieu, S., Fournier, R. a, & Renaud, J. (2015).  Generalizing predictive models of forest inventory attributes using an area-based approach with airborne las data. Remote Sensing of Environment, 156, 322-334. http://doi.org/10.1016/j.rse.2014.10.004
#'
#' @seealso \link[rTLS:voxels]{voxels}
#'
#' @examples
#' data("pc_tree")
#'
#' ###Create voxels of a size of 0.5.
#' vox <- voxels(pc_tree, voxel.size = 0.5)
#'
#' ###Estimate the vegetation structure
#' veg_structure(vox)
#'
#' @export
veg_structure <- function(voxels, k = 0.5, grid = FALSE) {

  layers <- voxels$voxels[, .(Points = sum(N)), by = Z]
  layers <- layers[order(Z)]
  prop <- round(layers$Points/sum(layers$Points), 6)
  prop <- c(prop, 0)
  sum_c <- cumsum(prop)
  gf <- shift(sum_c)/sum_c
  gf[is.na(gf)] = 0

  layers$GF <- gf[-length(gf)]
  layers$LAD <- round(-log(layers$GF)/(k*voxels$parameter), 6)
  layers[is.infinite(LAD)]$LAD = NA

  return(layers)
}
