#' @title Voxelization of a point cloud
#'
#' @description Create cubes of a given distance in a point cloud though their voxelization. It use a modify version of the code used in Greaves et al. 2015.
#'
#' @param cloud A \code{matrix} or \code{data.frame} with xyz coordinates in the first three columns.
#' @param voxel.size A positive \code{numeric} vector with the size of the voxel. It use the same dimentional scale of the point cloud.
#' @return If \code{obj.voxels = TRUE}, it return an object of class \code{voxels} wich is a list with the \code{cloud} and the parameter \code{voxel.size} used, and the \code{voxels} created. If \code{obj.voxel = FALSE} it return a data.frame with the voxels created.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#' @references Greaves, H. E., Vierling, L. A., Eitel, J. U., Boelman, N. T., Magney, T. S., Prager, C. M., & Griffin, K. L. (2015). Estimating aboveground biomass and leaf area of low-stature Arctic shrubs with terrestrial LiDAR. Remote Sensing of Environment, 164, 26-35.
#'
#' @examples
#' data("pc_tree")
#'
#' ###Create voxels of a size of 0.5.
#' voxels(pc_tree, voxel.size = 50)
#'
#' @export
voxels <- function(cloud, voxel.size, obj.voxels = TRUE) {

  colnames(cloud) <- c("X", "Y", "Z")
  vox <- cloud[,1:3]
  colnames(vox) <- c("X", "Y", "Z")

  max_digits <- max(decimals(vox$X), decimals(vox$Y), decimals(vox$Z)) ###Number of digist

  vox$X <- vox$X - min(cloud$X) ###Rescale to positive values
  vox$Y <- vox$Y - min(cloud$Y)
  vox$Z <- vox$Z - min(cloud$Z)

  min_point <- round(-voxel.size/2, digits = max_digits)   ##  Buffer the minimum point value by half the voxel size to find the lower bound for the XYZ voxels

  vox$X <- ceiling((vox$X - min_point)/voxel.size)
  vox$Y <- ceiling((vox$Y - min_point)/voxel.size)
  vox$Z <- ceiling((vox$Z - min_point)/voxel.size)

  vox <- vox[ , .N, by = .(X, Y, Z)] #Cound the number of points per voxel

  vox$X <- round(min(cloud[,1]) + ((vox$X-1)*voxel.size), max_digits) #Set coordinates
  vox$Y <- round(min(cloud[,2]) + ((vox$Y-1)*voxel.size), max_digits)
  vox$Z <- round(min(cloud[,3]) + ((vox$Z-1)*voxel.size), max_digits)

  if(obj.voxels == TRUE) {
    parameter <- voxel.size
    names(parameter) <- "voxel.size"
    final <- list(cloud = cloud, parameter = parameter, voxels = vox)
    class(final) <- "voxels"

  } else {
    final <- vox
  }
  return(final)
}

decimals <- function(x) {
  n <- 0
  while (!isTRUE(all.equal(floor(x),x)) & n <= 1e6) { x <- x*10; n <- n+1 }
  return (n)
}
