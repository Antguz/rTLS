#' @title Tree Trunk Volume
#'
#' @description Estimates the tree trunk volume of a point cloud using the \code{\link[alphashape3d]{ashape3d}} package.
#'
#' @param cloud A \code{data.table} with three columns representing the *XYZ* coordinates of a point cloud.
#' @param max.height A \code{numeric} vector to contemplate points in the cloud lower than a specific height. If \code{NULL}, it performs the alpha-shape on the entire point cloud.
#' @param alpha A \code{numeric} vector of length one passed to \code{ashape3d} to describes alpha. \code{alpha = 0.20} as default since it seems to provide better estimations of the trunk volume. However, the \code{alpha} value may depends on the resolution of the point cloud.
#' @param plot Logical. If \code{TRUE}, it uses \code{plot.ashape3d} to represent the alpha-shape.
#' @param ... General arguments passed to \code{ashape3d}.
#'
#' @details This is an adaptation of the code develop by Lafarge & Pateiro-Lopez (2017) based on Edelsbrunner & Mucke (1994) for the quick extraction of the tree trunk volume.
#' Therefore, if you use this code we kindly suggest to cite these documents in your research.
#'
#' @return A \code{numeric} vector with the estimated trunk volume.
#' @author J. Antonio Guzmán Q.
#'
#' @references Lafarge, T., Pateiro-Lopez, B. (2017). Implementation of the 3D Alpha-Shape for the Reconstruction of 3D Sets from a Point Cloud. Available at \url{https://CRAN.R-project.org/package=alphashape3d}.
#'
#' Edelsbrunner, H., Mucke, E. P. (1994). Three-Dimensional Alpha Shapes. ACM Transactions on Graphics, 13(1), pp.43-72.
#'
#' @seealso \code{\link{tree_metrics}}, \code{\link{circleRANSAC}}
#
#' @import alphashape3d
#'
#' @examples
#' data("pc_tree")
#'
#' #Estimates the trunk volume of a height lower than 1.75.
#' trunk_volume(pc_tree, max.height = 1.75)
#'
#' @export
trunk_volume <- function(cloud, max.height = NULL, alpha = 0.20, plot = TRUE, ...) {

  cloud <- cloud[,1:3]
  colnames(cloud) <- c("X", "Y", "Z")

  cloud <- cloud[Z <= max.height]

  shape <- ashape3d(as.matrix(cloud), alpha = alpha, ...)

  if(plot == TRUE) {
    plot3d(cloud)
    plot(shape, transparency = 0.5, clear = FALSE,  col = c("tan3", "tan3", "black"), walpha = FALSE, edges = FALSE)
  }

  volume <- volume_ashape3d(shape, indexAlpha = "all")
  names(volume) <- "Trunk volume"

  return(volume)
}
