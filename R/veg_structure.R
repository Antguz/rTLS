#' @title Vegetation structure
#'
#' @description Estimates the vegetation structure of the vertical profile of a point cloud based using voxel approach.
#'
#' @param voxels A \code{"voxels"} object.
#' @param k A coefficient of extinction. If \code{NULL}, 0.5 is assumed as a proxy to characterize the vegetation properties.
#'
#' @return If \code{obj.voxel = TRUE}, it return an object of class \code{"voxels"} wich contain a list with the points used to create the voxels, the parameter \code{voxel.size}, and the \code{voxels} created. If \code{obj.voxel = FALSE}, it returns a \code{data.table} with the coordinates of the voxels created and the number of points in each voxel.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#' @references Bouvier, M., Durrieu, S., Fournier, R. a, & Renaud, J. (2015).  Generalizing predictive models of forest inventory attributes using an area-based approach with airborne las data. Remote Sensing of Environment, 156, 322-334. http://doi.org/10.1016/j.rse.2014.10.004
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
veg_structure <- function(voxels, k = NULL) {

  nlayers <- unique(voxels$voxels$Z)
  results <- data.table(Z = nlayers)
  results$GF <- NA
  results$LAI <- NA
  results$LAD <- NA

  kc <- ifelse(is.null(k) == TRUE, 0.5, k)

  for(i in 1:nrow(results)) {

    GF <- sum(voxels$voxels[Z < nlayers[i]]$N) / (sum(voxels$voxels$N) - sum(voxels$voxels[Z > nlayers[i]]$N))
    LAI <- -(log(GF)/kc)
    LAD <- as.numeric(LAI/voxels$parameter)

    results$GF[i] <- GF
    results$LAI[i] <- ifelse(is.infinite(LAI), NA, LAI)
    results$LAD[i] <- ifelse(is.infinite(LAD), NA, LAD)
  }

  return(results)
}
