#' @title Plot Method for Voxels
#'
#' @description The \code{plot} method for objects of class \code{"voxels"} created using the \code{\link{voxels}} function.
#'
#' @aliases plot_voxels
#' @param voxels Object of class \code{"voxels"} from \code{\link{voxels}}.
#' @param add.points Logical, if \code{TRUE} it adds the original points used to perform the voxelization. \code{TRUE} as default.
#' @param add.voxels Logical, if \code{TRUE} it adds the voxels created. \code{TRUE} as default.
#' @param border Logical, if \code{TRUE} it adds a line on the borders of each voxel. \code{TRUE} as default.
#' @param points.size The points size, a positive number to use if plot \code{add.points = TRUE}.
#' @param points.col A \code{character} defining the color of the points to use.
#' @param fill.col A \code{character} vector defining the color to fill the voxels, it could be a range of colors or a solid color.
#' @param line.lwd The line width, a positive number, defaulting to 0.5.
#' @param line.col A \code{character} defining the color of the border lines to use.
#' @param alpha A positive numeric vector describing the transparency of the voxels to fill. This value most be between 0.0 (fully transparent) .. 1.0 (opaque).
#' @param ... General arguments passed to \code{\link{rgl}}.
#'
#' @return A 3D plot of a point cloud and voxels.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @seealso \code{\link{voxels}}, \code{\link{voxels_counting}}, \code{\link{summary_voxels}}
#'
#' @examples
#' data("pc_tree")
#'
#' ###Create cubes of a size of 7x7x3.5.
#' vox <- voxels(pc_tree, edge_length = c(7, 7, 3.5))
#' plot_voxels(vox)
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom rgl shade3d
#' @importFrom rgl scale3d
#' @importFrom rgl lines3d
#' @importFrom rgl cube3d
#' @importFrom rgl translate3d
#' @importFrom rgl points3d
#'
#' @export
plot_voxels <- function(voxels, add.points = TRUE, add.voxels = TRUE, border = TRUE, points.size = 1, points.col = "black", fill.col = "forestgreen", line.lwd = 0.5, line.col = "black", alpha = 0.10, ...) {

  if(class(voxels)[1] != "voxels") { ###Restriction to use
    stop("An object from voxels() need to be used")
  }

  voxels$voxels <-voxels$voxels[order(Z)] ###Construct plot in order

  if(add.voxels == TRUE) {

    cube <- cube3d() #Creates an empty voxel that will be used in the loop
    cube <- scale3d(cube, voxels$parameter[1]/2, voxels$parameter[2]/2, voxels$parameter[3]/2) #Modify the voxel size using all the digits of the XYZ voxels
    cube$material$lwd <- line.lwd
    cube$material$front <- 'line'
    cube$material$back <- 'line'

    colfunc <- colorRampPalette(fill.col)
    col_to_use <- colfunc(max(voxels$voxels$N))
    col_to_use <- col_to_use[voxels$voxels$N]


    for(i in 1:nrow(voxels$voxels)) {

      box <- cube ###Creates the lines around the voxels
      box <- translate3d(box, voxels$voxels$X[i], voxels$voxels$Y[i], voxels$voxels$Z[i])
      shade3d(box, col= col_to_use[i], alpha = alpha)

      if(border == TRUE) {
        for (i in 1:6) {
          lines3d(t(box$vb)[box$ib[,i],], size = line.lwd, col = line.col)
        }
      }
    }
  }

  if(add.points == TRUE) {
    points3d(voxels$cloud$X, voxels$cloud$Y, voxels$cloud[,3]$Z, size = points.size, col = points.col)
  }
}

