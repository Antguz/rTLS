#' @import dplyr
#'
#' @title Voxelization of a point cloud
#'
#' @description Create cubes of a given distance in a point cloud though their voxelization. It use a modify version of the code used in Greaves et al. 2015.
#'
#' @param cloud A \code{matrix} or \code{data.frame} with xyz coordinates in the first three columns.
#' @param voxel.size A \code{numeric} vector with the size of the voxel. It use the same dimentional scale of the point cloud.
#' @return A \code{data.frame} with the xyz coordinates of the voxels created and a fourth column with the amount of points in a given voxel.
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
voxels <- function(cloud, voxel.size, precision) {

  xi <- round(min(cloud[,1]) - voxel.size/2, digits = precision)    ##  Buffer the minimum point value by half the voxel size to find the lower bound for the x,y, and z voxels
  yi <- round(min(cloud[,2]) - voxel.size/2, digits = precision)
  zi <- round(min(cloud[,3]) - voxel.size/2, digits = precision)

  cloud$x_vox <- ceiling((cloud[,1]-xi)/voxel.size)    ##  Assign x, y, and z "voxel coordinates" to each point as a point attribute
  cloud$y_vox <- ceiling((cloud[,2]-yi)/voxel.size)
  cloud$z_vox <- ceiling((cloud[,3]-zi)/voxel.size)

  voxels.cloud <- cloud %>% count(x_vox, y_vox, z_vox, sort = TRUE) #Cound the number of points per voxel
  as.data.frame(voxels.cloud)

}
