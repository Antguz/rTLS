#' @title Voxelization of a point cloud
#'
#' @description Create cubes of a given distance in a point cloud though their voxelization. It use a modify version of the code used in Greaves et al. 2015.
#'
#' @param cloud A \code{matrix} or \code{data.frame} with _xyz_ coordinates in the first three columns.
#' @param voxel.size A positive \code{numeric} vector with the size of the voxel. It use the same dimentional scale of the point cloud.
#' @param random Logical. If \code{TRUE}, it generates voxels on a set of random points created using the same number of points and _xyz_ range of \code{cloud}. \code{FALSE} as default.
#' @param obj.voxels Logical. If \code{obj.voxel = TRUE}, it returns an object of class \code{"voxels"}, If \code{obj.voxel = FALSE}, it returns a \code{data.table} with the coordinates of the voxels created and the number of points in each voxel. \code{TRUE} as default
#'
#' @return If \code{obj.voxel = TRUE}, it return an object of class \code{"voxels"} wich contain a list with the points used to create the voxels, the parameter \code{voxel.size}, and the \code{voxels} created. If \code{obj.voxel = FALSE}, it returns a \code{data.table} with the coordinates of the voxels created and the number of points in each voxel.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#' @references Greaves, H. E., Vierling, L. A., Eitel, J. U., Boelman, N. T., Magney, T. S., Prager, C. M., & Griffin, K. L. (2015). Estimating aboveground biomass and leaf area of low-stature Arctic shrubs with terrestrial LiDAR. Remote Sensing of Environment, 164, 26-35.
#'
#' @examples
#' data("pc_tree")
#'
#' ###Create voxels of a size of 0.5.
#' voxels(pc_tree, voxel.size = 0.5)
#'
#' ###Returns just the coordinates of the voxels and the number of points in each voxel
#' voxels(pc_tree, voxel.size = 0.5, obj.voxel = FALSE)
#'
#' ###Create a random cloud based on a point cloud and return voxels
#' voxels(pc_tree, voxel.size = 0.5, random = TRUE)
#'
#' @export
voxels <- function(cloud, voxel.size, random = FALSE, obj.voxels = TRUE) {

  cloud <- cloud[,1:3]
  colnames(cloud) <- c("X", "Y", "Z")

  if(random == FALSE) {
    vox <- cloud

  } else if(random == TRUE) {
    vox <- data.table(X = runif(nrow(cloud), min(cloud$X), max(cloud$X)),
                      Y = runif(nrow(cloud), min(cloud$Y), max(cloud$Y)),
                      Z = runif(nrow(cloud), min(cloud$Z), max(cloud$Z)))
    vox_final <- vox
  }

  max_digits <- max(decimals(cloud$X), decimals(cloud$Y), decimals(cloud$Z)) ###Number of digist
  min_point <- as.numeric(paste("0.", paste(format(rep(0, max_digits-1)), collapse = ''), "1", sep = ""))   ##  Buffer the minimum point value by half the voxel size to find the lower bound for the XYZ voxels

  vox$X <- round((vox$X - min(cloud$X)) + min_point, max_digits) ###Rescale to positive values
  vox$Y <- round((vox$Y - min(cloud$Y)) + min_point, max_digits)
  vox$Z <- round((vox$Z - min(cloud$Z)) + min_point, max_digits)

  vox$X <- round(ceiling(vox$X/voxel.size), max_digits)
  vox$Y <- round(ceiling(vox$Y/voxel.size), max_digits)
  vox$Z <- round(ceiling(vox$Z/voxel.size), max_digits)

  vox <- vox[ , .N, by = .(X, Y, Z)] #Cound the number of points per voxel

  vox$X <- round((min(cloud[,1]) + ((vox$X-1)*voxel.size) + (voxel.size/2)), max_digits) #Set coordinates
  vox$Y <- round((min(cloud[,2]) + ((vox$Y-1)*voxel.size) + (voxel.size/2)), max_digits)
  vox$Z <- round((min(cloud[,3]) + ((vox$Z-1)*voxel.size)) + (voxel.size/2), max_digits)

  if(obj.voxels == TRUE) {
    parameter <- voxel.size
    names(parameter) <- "voxel.size"

    if(random == FALSE) {
      final <- list(cloud = cloud, parameter = parameter, voxels = vox)

    } else if(random == TRUE) {
      final <- list(cloud = vox_final, parameter = parameter, voxels = vox)
    }

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
