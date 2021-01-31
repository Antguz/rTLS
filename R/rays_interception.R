#' @title Intersection of Rays
#'
#' @description Intersection of rays by voxels
#'
#' @param orig A \code{data.table} with the describing *XYZ* coordinates of the the start path of the rays.
#' @param dir A \code{data.table} with the describing *XYZ* coordinates of the the end path of the rays.
#' @param voxels A \code{numeric} vector with *XYZ* coordinates of the center of voxels.
#' @param edge_length A positive \code{numeric} vector with the voxel-edge length for the X, Y, and Z coordinates.
#'
#' @return It returns a \code{data.table} with three columns: i) number of points
#' that were not intercepted (NI), ii) number of points intercepted without exit (INE), and
#' iii) number of points that were intercepted and present an exit (IE). The number of rows
#' match with \code{nrow(voxels)}.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @seealso \code{\link{ray_AABB}}, \code{\link{voxels}}
#'
#' @import data.table
#'
#' @examples
#'
#' #Create points with paths
#' n <- 20
#' orig <- data.table(X = runif(n, min = -5, max = 5),
#'                    Y = runif(n, min = -5, max = 5),
#'                    Z = runif(n, min = -5, max = 5))
#'
#' dir <- data.table(X = runif(n, min = -5, max = 5),
#'                   Y = runif(n, min = -5, max = 5),
#'                   Z = runif(n, min = -5, max = 5))
#'
#' #Create a potential voxel
#' voxel <- data.table(X = 0, Y = 0, Z = 0)
#' edge_length <- c(2, 2, 2)
#'
#' #Plot
#' cube <- rgl::cube3d()
#' cube <- rgl::scale3d(cube, edge_length[1]/2, edge_length[2]/2, edge_length[3]/2)
#' box <- rgl::translate3d(cube, voxel[[1]], voxel[[2]], voxel[[3]])
#' rgl::shade3d(box, col= "green", alpha = 0.6)
#' rgl::points3d(orig, size = 5, col = "black")
#' rgl::points3d(dir, size = 5, col = "red")
#'
#' for(i in 1:nrow(orig)) {
#' rgl::lines3d(c(orig[[1]][i], dir[[1]][i]),
#'              c(orig[[2]][i], dir[[2]][i]),
#'              c(orig[[3]][i], dir[[3]][i]), col = "blue")
#' }
#'
#' #Estimation
#' rays_interception(orig, dir, voxel, edge_length, progress = FALSE)
#'
#' @export
rays_interception <- function(orig, dir, voxels, edge_length, threads = 1, progress = TRUE) {

  if(nrow(orig) != nrow(dir)) {
    stop("The nrow() between orig and dir does not match, each ray must have a starting and ending point")
  }

  results <- rays_interception_rcpp(as.matrix(orig), as.matrix(dir), as.matrix(voxels), edge_length, threads, progress)
  results <- as.data.table(results)
  colnames(results) <- c("NI", "INE", "IE")

  return(results)
}
