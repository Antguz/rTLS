#' @import dplyr
#'
#' @title Voxelization of a point cloud
#'
#' @description Create cubes of a given distance in a point cloud though their voxelization. It use a modify version of the code used in Greaves et al. 2015.
#'
#' @param cloud A \code{matrix} or \code{data.frame} with xyz coordinates in the first three columns.
#' @param voxel.size A positive \code{numeric} vector with the size of the voxel. It use the same dimentional scale of the point cloud.
#' @param obj.voxel Logical, if \code{obj.voxel = TRUE} it return an object of class \code{voxels}. If \code{obj.voxel = FALSE} it return a data.frame with the voxels created. \code{obj.voxel = TRUE} as default.
#' @return If \code{obj.voxel = TRUE}, it return an object of class \code{voxels} wich is a list with the \code{cloud} and the parameter \code{voxel.size} used, and the \code{voxels} created. If \code{obj.voxel = FALSE} it return a data.frame with the voxels created.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#' @references Greaves, H. E., Vierling, L. A., Eitel, J. U., Boelman, N. T., Magney, T. S., Prager, C. M., & Griffin, K. L. (2015). Estimating aboveground biomass and leaf area of low-stature Arctic shrubs with terrestrial LiDAR. Remote Sensing of Environment, 164, 26-35.
#'
#' @examples
#' data("pc_tree")
#'
#' ###Create voxels of a size of 0.5.
#' voxels(pc_tree, voxel.size = 0.5)
#'
#'@export
voxels <- function(cloud, voxel.size, obj.voxel = TRUE) {

  vox <- cloud[,1:3]
  colnames(vox) <- c("X", "Y", "Z")

  max_digits <- max(decimals(vox$X), decimals(vox$Y), decimals(vox$Z))

  min_point <- c(round(min(vox[,1]) - voxel.size/2, digits = max_digits),    ##  Buffer the minimum point value by half the voxel size to find the lower bound for the x,y, and z voxels
                 round(min(vox[,2]) - voxel.size/2, digits = max_digits),
                 round(min(vox[,3]) - voxel.size/2, digits = max_digits))

  vox <- vox[, c("X", "Y", "Z") := c(ceiling((cloud[,1] - min_point[1])/voxel.size), ##  Assign x, y, and z "voxel coordinates" to each point as a point attribute
                                   ceiling((cloud[,2]- min_point[2])/voxel.size),
                                   ceiling((cloud[,3]- min_point[3])/voxel.size)), by = .I]

  vox <- vox[ , .N, by = .(X, Y, Z)] #Cound the number of points per voxel

  vox$X <- round((min(cloud[,1]) - voxel.size/2) + (vox$X*voxel.size), max_digits) #Set coordinates
  vox$Y <- round((min(cloud[,2]) - voxel.size/2) + (vox$Y*voxel.size), max_digits)
  vox$Z <- round((min(cloud[,3]) - voxel.size/2) + (vox$Z*voxel.size), max_digits)

  if(obj.voxel == TRUE) {
    parameter <- voxel.size
    names(parameter) <- "voxel.size"
    final <- list(cloud = cloud, parameter = parameter, voxels = vox)
    class(final) <- "voxels"

  } else if(obj.voxel == FALSE) {
    final <- vox
  }
  return(final)
}

decimals <- function(x) {
  n <- 0
  while (!isTRUE(all.equal(floor(x),x)) & n <= 1e6) { x <- x*10; n <- n+1 }
  return (n)
}
