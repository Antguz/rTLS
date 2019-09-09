#' @title Plot Method for Voxels
#'
#' @description The \code{plot} method for objects of class \code{"voxels"} created using the \code{\link{voxels}} function.
#'
#' @aliases plot_voxels
#' @param voxels Object of class \code{"voxels"} from \code{\link{voxels}}.
#' @param add.points Logical, if \code{TRUE} it adds the original points used to perform the voxalization. \code{TRUE} as default.
#' @param add.voxels Logical, if \code{TRUE} it adds the voxels created. \code{TRUE} as default.
#' @param border Logical, if \code{TRUE} it adds a line on the borders of each voxel. \code{TRUE} as default.
#' @param fill Logical, if \code{TRUE} it fills each voxels with colors. \code{TRUE} as default.
#' @param gradientcol Logical, if \code{TRUE} it uses a gradient of colors based on the number of points in each voxels. \code{FALSE} as default.
#' @param points.size The points size, a positive number to use if plot \code{add.points = TRUE}.
#' @param pointscol A \code{character} definting the color of the points to use.
#' @param fillcol A \code{character} definting the color to fill the voxels.
#' @param lwd The line width, a positive number, defaulting to 0.5.
#' @param alpha A positive numeric vector describing the transparency of the voxels to fill. This value most be between 0.0 (fully transparent) .. 1.0 (opaque).
#' @param ... General arguments passed to \code{\link{rgl}}.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @seealso \code{\link{voxels}}, \code{\link{voxels_counting}}, \code{\link{summary_voxels}}
#'
#' @examples
#' data("pc_tree")
#'
#' ###Create voxels of a size of 0.5.
#' vox <- voxels(pc_tree, voxel.size = 0.5)
#' plot_voxels(vox)
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom rgl plot3d
#' @importFrom rgl shade3d
#' @importFrom rgl cube3d
#' @importFrom rgl %>%
#' @importFrom rgl translate3d
#' @importFrom rgl points3d
#'
#' @export
plot_voxels <- function(voxels, add.points = TRUE, add.voxels = TRUE, border = TRUE, fill = TRUE, gradientcol = FALSE, points.size = 1, pointscol = "black", fillcol = "forestgreen", lwd = 0.5, alpha = 0.10, ...) {

  if(class(voxels)[1] != "voxels") {
    stop("An object from voxels() need to be used")
  }

  if(gradientcol == TRUE) {
    colfunc <- colorRampPalette(c("royalblue", "springgreen", "yellow", "red"))
    col_to_use <- colfunc(max(max(voxels$voxels$N)))
  }

  voxels$voxels <-voxels$voxels[order(Z)] ###Construct in order

  if(add.voxels == TRUE) {

    cube <- cube3d() #Creates an empy voxel that will be used in the loop

    cube$vb[4,] <- cube$vb[4,]/(voxels$parameter/2) #Modify the voxel size using all the digits of the XYZ voxels

    for(i in 1:nrow(voxels$voxels)) {

      if(border == TRUE) {

        box <- cube ###Creates the lines arround the voxels
        box$material$lwd <- lwd
        box$material$front <- 'line'
        box$material$back <- 'line'
        box %>% translate3d(voxels$voxels$X[i], voxels$voxels$Y[i], voxels$voxels$Z[i]) %>% shade3d
      }

      if(fill == TRUE) {

        if(gradientcol == FALSE) {
        color_box <- cube ###Fill the voxels using colors
        color_box$material$alpha <- alpha
        color_box$material$col <- fillcol
        color_box$vb[4,] <- color_box$vb[4,]*1.0000001
        color_box %>% translate3d(voxels$voxels$X[i], voxels$voxels$Y[i], voxels$voxels$Z[i]) %>% shade3d

        } else if(gradientcol == TRUE) {
        color_box <- cube ###Fill the voxels using colors
        color_box$material$alpha <- alpha
        color_box$material$col <- col_to_use[voxels$voxels$N[i]]
        color_box$vb[4,] <- color_box$vb[4,]*1.0000001
        color_box %>% translate3d(voxels$voxels$X[i], voxels$voxels$Y[i], voxels$voxels$Z[i]) %>% shade3d
      }
    }
    }
  }

  if(add.points == TRUE) {
    points3d(voxels$cloud$X, voxels$cloud$Y, voxels$cloud[,3]$Z, size = points.size, col = pointscol)
  }
}
