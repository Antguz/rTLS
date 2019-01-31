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
#' data("test")
#'
#'
#'
#' ###Create voxels of a size of 0.5.
#' voxels(pc_tree, voxel.size = 0.5)
#'
#'@export
voxels <- function(cloud, voxel.size, obj.voxels = TRUE) {

  vox <- cloud[,1:3]
  colnames(vox) <- c("X", "Y", "Z")

  max_digits <- max(decimals(vox$X), decimals(vox$Y), decimals(vox$Z))

  min_point <- c(round(min(cloud[,1]) - voxel.size/2, digits = max_digits),    ##  Buffer the minimum point value by half the voxel size to find the lower bound for the XYZ voxels
                 round(min(cloud[,2]) - voxel.size/2, digits = max_digits),
                 round(min(cloud[,3]) - voxel.size/2, digits = max_digits))

  vox$X <- ifelse(min_point[1] < 0, ceiling((cloud[,1] + min_point[1])/voxel.size),  ceiling((cloud[,1] - min_point[1])/voxel.size)) ###Create intergers
  vox$Y <- ifelse(min_point[2] < 0, ceiling((cloud[,2] + min_point[2])/voxel.size),  ceiling((cloud[,2] - min_point[2])/voxel.size))
  vox$Z <- ifelse(min_point[3] < 0, ceiling((cloud[,3] + min_point[3])/voxel.size),  ceiling((cloud[,3] - min_point[3])/voxel.size))

  vox <- vox[ , .N, by = .(X, Y, Z)] #Cound the number of points per voxel

  vox$X <- ((vox$X - min(vox$X))/(max(vox$X)-min(vox$X))) * (length(unique(vox$X))-1) ###Rescale intergers
  vox$Y <- ((vox$Y - min(vox$Y))/(max(vox$Y)-min(vox$Y))) * (length(unique(vox$Y))-1)
  vox$Z <- ((vox$Z - min(vox$Z))/(max(vox$Z)-min(vox$Z))) * (length(unique(vox$Z))-1)

  vox$X <- round(min(cloud[,1]) +(vox$X*voxel.size), max_digits) #Set coordinates
  vox$Y <- round(min(cloud[,2]) + (vox$Y*voxel.size), max_digits)
  vox$Z <- round(min(cloud[,3]) + (vox$Z*voxel.size), max_digits)

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
