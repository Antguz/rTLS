#' @title Plot method for voxels
#'
#' @description The \code{plot} method for voxels created on point clouds using \code{voxels()} function.
#'
#' @param voxels A \code{"voxels"} object.
#' @param add.points Logical, if \code{TRUE} it add the original points used to perform the voxalization. \code{TRUE} as default.
#' @param points.size The points size, a positive number to use if plot \code{add.points = TRUE}.
#' @param add.voxels Logical, if \code{TRUE} it add the voxels created. \code{TRUE} as default.
#' @param border Logical, if \code{TRUE} it add a line on the borders of each voxel. \code{TRUE} as default.
#' @param fill Logical, if \code{TRUE} it fill each voxels with colors. \code{TRUE} as default.
#' @param fillcol A \code{character} definting the color to fill the voxels.
#' @param bordercol A \code{character} definting the color of the border of each voxels.
#' @param lwd The line width, a positive number, defaulting to 0.5.
#' @param alpha A positive numeric vector describing the transparency of the voxels to fill. This value most be between 0.0 (fully transparent) .. 1.0 (opaque).
#' @param ... General arguments passed to rgl.
#' @param voxel.size A positive \code{numeric} vector with the size of the voxel. It use the same dimentional scale of the point cloud.

#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @seealso \code{voxels()}
#'
#' @examples
#' data("pc_tree")
#'
#' ###Create voxels of a size of 0.5.
#' vox <- voxels(pc_tree, voxel.size = 0.5)
#' plot.voxels(vox)
#'
#'@export

plot.voxels <- function(voxels, add.points = TRUE, points.size = 0.1, add.voxels = TRUE, border = TRUE, fill = TRUE, fillcol = "forestgreen", bordercol = "gray20", lwd = 0.5, alpha = 0.10, ...) {

  if(class(voxels)[1] != "voxels") {
    stop("An object from voxels() need to be used")
  }

  voxels$voxels <-voxels$voxels[order(Z)]

  if(add.voxels == TRUE) {

    cube <- cube3d() #Creates an empy voxel that will be used in the loop

    cube$vb[4,] <- cube$vb[4,]/voxels$parameter #Modify the voxel size using all the digits of the XYZ voxels

    voxels$voxels <- voxels$voxels[order(Z)]

    for(i in 1:nrow(voxels$voxels)) {

      if(border == TRUE) {

        box <- cube ###Creates the lines arround the voxels
        box$material$lwd <- lwd
        box$material$front <- 'line'
        box$material$back <- 'line'
        box$material$col <- bordercol
        box %>% translate3d(voxels$voxels$X[i], voxels$voxels$Y[i], voxels$voxels$Z[i]) %>% shade3d
      }

      if(fill == TRUE) {

        color_box <- cube ###Fill the voxels using colors
        color_box$material$alpha <- alpha
        color_box$material$col <- fillcol
        color_box$vb[4,] <- color_box$vb[4,]*1.0000001
        color_box %>% translate3d(voxels$voxels$X[i], voxels$voxels$Y[i], voxels$voxels$Z[i]) %>% shade3d

      }
    }
  }

  if(add.points == TRUE) {
    points3d(voxels$cloud$X, voxels$cloud$Y, voxels$cloud[,3]$Z, size = points.size)
  }
}
