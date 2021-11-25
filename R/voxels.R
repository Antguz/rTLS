#' @title Voxelization of a Point Cloud
#'
#' @description Create cubes of a given distance in a point cloud though their voxelization. It use a modify version of the code used in Greaves et al. 2015.
#'
#' @param cloud A \code{data.table} with *XYZ* coordinates in the first three columns.
#' @param edge_length A positive \code{numeric} vector with the voxel-edge length for the x, y, and z coordinates. It use the same dimensional scale of the point cloud.
#' @param threads An \code{integer} specifying the number of threads to use for parallel processing. Experiment to see what works best for your data on your hardware.
#' @param obj.voxels Logical. If \code{obj.voxel = TRUE}, it returns an object of class \code{"voxels"}, If \code{obj.voxel = FALSE}, it returns a \code{data.table} with the coordinates of the voxels created and the number of points in each voxel. \code{TRUE} as default.
#'
#' @details Voxels are created from the negative to the positive *XYZ* coordinates.
#'
#' @return If \code{obj.voxels == TRUE}, it return an object of class \code{"voxels"} which contain a list with the points used to create the voxels, the parameter \code{edge_length}, and the \code{voxels} created. If \code{FALSE}, it returns a \code{data.table} with the coordinates of the voxels created and the number of points in each voxel.
#' @author J. Antonio Guzmán Q.
#'
#' @references Greaves, H. E., Vierling, L. A., Eitel, J. U., Boelman, N. T., Magney, T. S., Prager, C. M., & Griffin, K. L. (2015). Estimating aboveground biomass and leaf area of low-stature Arctic shrubs with terrestrial LiDAR. Remote Sensing of Environment, 164, 26-35.
#'
#' @seealso \code{\link{voxels_counting}}, \code{\link{plot_voxels}}, \code{\link{summary_voxels}}
#'
#' @import data.table
#'
#' @examples
#' data("pc_tree")
#'
#' ###Create cube of a size of 0.5.
#' voxels(pc_tree, edge_length = c(0.5, 0.5, 0.5))
#'
#'
#' @export
voxels <- function(cloud, edge_length, threads = 1L, obj.voxels = TRUE) {

  vox <- as.data.table(voxelization_rcpp(as.matrix(cloud), edge_length, threads))
  colnames(vox) <- c("X", "Y", "Z")

  vox <- vox[ , .N, by = .(X, Y, Z)] #Count the number of points per voxel

  if(obj.voxels == TRUE) {
    parameter <- edge_length
    names(parameter) <- c("X.size", "Y.size", "Z.size")

    final <- list(cloud = cloud, parameter = parameter, voxels = vox)
    class(final) <- "voxels"

  } else {
    final <- vox
  }
  return(final)
}
