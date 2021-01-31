#' @title Ray Bounding Box
#'
#' @description Intersection of a ray by a bounding box
#'
#' @param orig A \code{data.table} with the describing *XYZ* coordinates of the the start path of a ray.
#' @param dir A \code{data.table} with the describing *XYZ* coordinates of the the end path of a ray.
#' @param voxel_min A \code{numeric} vector with the minimum *XYZ* coordinates of the voxel edges.
#' @param voxel_max A \code{numeric} vector with the maximum *XYZ* coordinates of the voxel edges.
#'
#' @return An \code{interger}, if \code{0} the point is not intercepted by the voxel. If \code{1} the point is intercepted by the voxel, but it does not exit,
#' If \code{2} the point is intercepted by the voxel and exit.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @seealso \code{\link{voxels}}, \code{\link{plot_voxels}}, \code{\link{summary_voxels}}
#'
#' @import data.table
#'
#' @examples
#'
#' #Create points, paths, and a voxel
#' orig <- data.table(X = c(1, 1, 1), Y = c(0.75, 1, 1.25), Z = c(1, 1, 1))
#' dir <- data.table(X = c(1, 1, 1), Y = c(0.75, 1, 1.25), Z = c(2.5, 4, 1.5))
#' voxel_cor <- matrix(c(1, 1, 2.5), ncol = 3)
#' edge_length <- c(1, 1, 1)
#' voxel_min <- c(voxel_cor[1, 1] - edge_length[1]/2,
#'                voxel_cor[1, 2] - edge_length[2]/2,
#'                voxel_cor[1, 3] - edge_length[3]/2)
#' voxel_max <- c(voxel_cor[1, 1] + edge_length[1]/2,
#'                voxel_cor[1, 2] + edge_length[2]/2,
#'                voxel_cor[1, 3] + edge_length[3]/2)
#'
#' #Plot
#' cube <- rgl::cube3d()
#' cube <- rgl::scale3d(cube, edge_length[1]/2, edge_length[2]/2, edge_length[3]/2)
#' box <- rgl::translate3d(box, voxel_cor[1, 1], voxel_cor[1, 2], voxel_cor[1, 3])
#' rgl::shade3d(box, col= "green", alpha = 0.6)
#' rgl::points3d(orig, size = 4, col = "black")
#' rgl::points3d(dir, size = 4, col = "black")
#'
#' #Line intercepted
#' rgl::lines3d(c(orig[1, 1], dir[1, 1]),
#'              c(orig[1, 2], dir[1, 2]),
#'              c(orig[1, 3], dir[1, 3]), col = "red")
#'
#' ray_AABB(orig[1,], dir[1,], voxel_min, voxel_max)
#'
#' #Line intercepted with exit
#' rgl::lines3d(c(orig[2, 1], dir[2, 1]),
#'              c(orig[2, 2], dir[2, 2]),
#'              c(orig[2, 3], dir[2, 3]), col = "blue")
#'
#' ray_AABB(orig[2,], dir[2,], voxel_min, voxel_max)
#'
#' #Non-intercepted line
#' rgl::lines3d(c(orig[3, 1], dir[3, 1]),
#'              c(orig[3, 2], dir[3, 2]),
#'              c(orig[3, 3], dir[3, 3]), col = "black")
#'
#' ray_AABB(orig[3,], dir[3,], voxel_min, voxel_max)
#'
#' @export
ray_AABB <- function(orig, dir, voxel_min, voxel_max) {

  results <- ray_AABB_rcpp(as.matrix(orig), as.matrix(dir), voxel_min, voxel_max)

  return(results)
}

