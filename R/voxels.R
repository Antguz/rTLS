#' @title Voxelization of a point cloud
#'
#' @description Create cubes of a given distance in a point cloud though their voxelization. It use a modify version of the code used in Greaves et al. 2015.
#'
#' @param cloud A \code{matrix} or \code{data.frame} with _xyz_ coordinates in the first three columns.
#' @param voxel.size A positive \code{numeric} vector with the size of the voxel. It use the same dimentional scale of the point cloud.
#' @param obj.voxels Logical. If \code{obj.voxel = TRUE}, it returns an object of class \code{"voxels"}, If \code{obj.voxel = FALSE}, it returns a \code{data.table} with the coordinates of the voxels created and the number of points in each voxel. \code{TRUE} as default.
#' @param random Logical. If \code{TRUE}, it generates voxels on a random point cloud created using the same number of points and _xyz_ range of \code{cloud}, and a minimun distance between points. \code{FALSE} as default.
#' @param minDistance A positive \code{numeric} vector describing the minimun distance between points. This need to be used if \code{random = TRUE}. If \code{random = TRUE} and \code{minDistance = NULL}, it automatically estimate the minimun distance using \code{min_distance()}.
#' @param parallel Logical. If \code{TRUE}, it estimate the \code{minDistance} using parallel processing if \code{minDistance = NULL}.
#' @param cores An \code{integer} >= 0 describing the number of cores use. This need to be used if \code{parallel = TRUE}.
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
#' ###Create a random cloud based on a previus point cloud with a known minimun point distance of 0.05 and return voxels
#' voxels(pc_tree, voxel.size = 0.5, random = TRUE, minDistance = 0.05)
#'
#' ###Create a random cloud based on a previus point cloud with a unknown minimun point distance and return voxels
#' voxels(pc_tree, voxel.size = 0.5, random = TRUE, minDistance = 0.05)
#'
#' ###Create a random cloud based on a previus point cloud with a unknown minimun point distance that will be estimated using parallel processing.
#' voxels(pc_tree, voxel.size = 0.5, random = TRUE, parallel = TRUE, cores = 4)
#'
#' @export
voxels <- function(cloud, voxel.size, obj.voxels = TRUE, random = FALSE, minDistance = NULL, parallel = FALSE, cores = NULL) {

  cloud <- cloud[,1:3]
  colnames(cloud) <- c("X", "Y", "Z")

  if(random == FALSE) {  ###If the voxelization is performed on a non-random point cloud
    vox <- cloud

  } else if(random == TRUE) { ###If the voxelization is performed on a random point cloud

    if(is.null(minDistance) == TRUE) { ####If minimun distance is not defined
      point_min_distance <- min_distance(cloud, parallel = parallel, cores = cores)
    } else {
      point_min_distance <- minDistance ####If minimun distance is defined
    }

    range <- c(min(cloud$X), max(cloud$X), min(cloud$Y), max(cloud$Y), min(cloud$Z), max(cloud$Z)) ###Range of the point cloud

    vox <- random_cloud(nrow(cloud), range, point_min_distance) ###Creation of the random point cloud

    vox_final <- vox
  }

  max_digits <- max(decimals(cloud$X), decimals(cloud$Y), decimals(cloud$Z)) ###Number of digist
  min_point <- as.numeric(paste("0.", paste(format(rep(0, max_digits-1)), collapse = ''), "1", sep = ""))   ##  Buffer the minimum point value

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
