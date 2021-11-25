#' @title Intersection of Lines by AABBs
#'
#' @description Intersection of lines by several Axis-Aligned Bounding Boxs.
#'
#' @param orig A \code{data.table} with the describing *XYZ* coordinates of the the start path of the rays.
#' @param end A \code{data.table} with the describing *XYZ* coordinates of the the end path of the rays.
#' @param AABBs A \code{data.table} with *XYZ* coordinates of the center of AABBs.
#' @param edge_length A positive \code{numeric} vector with the AABB length edge for the X, Y, and Z coordinates.
#' @param threads An \code{integer} >= 0 describing the number of threads to use. This need to be used if \code{parallel = TRUE}.
#' @param progress Logical, if \code{TRUE} displays a graphical progress bar. \code{TRUE} as default.
#'
#' @return It returns a \code{data.table} with nine columns: 1-5 columns with
#' the counts for the code of intersection (see \code{\link{line_AABB}}), and
#' 6-9 columns with sum the path length of intersection. The number of rows match
#' with \code{nrow(AABBs)}.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @seealso \code{\link{line_AABB}}, \code{\link{voxels}}
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
#' end <- data.table(X = runif(n, min = -5, max = 5),
#'                   Y = runif(n, min = -5, max = 5),
#'                   Z = runif(n, min = -5, max = 5))
#'
#' #Create a potential AABB
#' AABBs <- data.table(X = 0, Y = 0, Z = 0)
#' edge_length <- c(2, 2, 2)
#'
#' #Plot
#' \donttest{
#' cube <- rgl::cube3d()
#' cube <- rgl::scale3d(cube,
#'                      edge_length[1]/2,
#'                      edge_length[2]/2,
#'                      edge_length[3]/2)
#' box <- rgl::translate3d(cube, AABBs[[1]], AABBs[[2]], AABBs[[3]])
#' rgl::shade3d(box, col= "green", alpha = 0.6)
#' rgl::points3d(orig, size = 5, col = "black")
#' rgl::points3d(end, size = 5, col = "red")
#'
#' for(i in 1:nrow(orig)) {
#' rgl::lines3d(c(orig[[1]][i], end[[1]][i]),
#'              c(orig[[2]][i], end[[2]][i]),
#'              c(orig[[3]][i], end[[3]][i]), col = "grey")
#' }
#' }
#'
#' #Estimation
#' lines_interception(orig, end, AABBs, edge_length, progress = FALSE)
#'
#' @export
lines_interception <- function(orig, end, AABBs, edge_length, threads = 1, progress = TRUE) {

  if(nrow(orig) != nrow(end)) {
    stop("The nrow() between orig and end does not match, each ray must have a starting and ending point")
  }

  results <- lines_interception_rcpp(as.matrix(orig), as.matrix(end), as.matrix(AABBs), edge_length, threads, progress)
  results <- as.data.table(results)
  colnames(results) <- c("code_0", "code_1", "code_2", "code_3", "code_4", "path_1", "path_2", "path_3", "path_4")

  return(results)
}
