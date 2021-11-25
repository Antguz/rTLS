#' @title Line-AABB
#'
#' @description Intersection of a line by an Axis-Aligned Bounding Box.
#'
#' @param orig A \code{data.table} with the describing *XYZ* coordinates of the the start path of a line.
#' @param end A \code{data.table} with the describing *XYZ* coordinates of the the end path of a line.
#' @param AABB_min A \code{numeric} vector with the minimum *XYZ* coordinates of the AABB
#' @param AABB_max A \code{numeric} vector with the maximum *XYZ* coordinates of the AABB.
#'
#' @return An numeric \code{vector} of length two, describing if the line was
#' intercepted or not, and the length of the intercepted line within in the AABB.
#' See details.
#'
#' @details The interaction of a line with a AABB may result in five scenarios:
#' i) the line is not intercepted by a AAABB (\code{0}), ii) the origin and end
#' of the line falls within the AABB (\code{1}), iii) the origin point of the
#' line falls within the AABB both not the end point (\code{2}), iv) the end point
#' of the line falls within the AABB both not the origin point (\code{3}), and
#' v) the line is intercepted by the AABB (\code{4}).
#'
#' @author J. Antonio Guzmán Q.
#'
#' @seealso \code{\link{lines_interception}}, \code{\link{voxels}},
#'
#' @import data.table
#'
#' @examples
#'
#' #Create origins and end paths
#' orig <- data.table(X = c(0, 0, 0, 0, 0),
#'                    Y = c(-0.45, -0.25, 0, 0.25, 0.45),
#'                    Z = c(-1, -0.25, 0, -1, -1))
#'
#' end <- data.table(X = c(0, 0, 0, 0, 0),
#'                   Y = c(-0.45, -0.25, 0, 0.25, 0.45),
#'                   Z = c(-0.75, 0.25, 1, 0, 1))
#'
#' #Create the AABB
#' AABB <- matrix(c(0, 0, 0), ncol = 3)
#' edge_length <- c(1, 1, 1)
#'
#' AABB_min <- c(AABB[1, 1] - edge_length[1]/2,
#'               AABB[1, 2] - edge_length[2]/2,
#'               AABB[1, 3] - edge_length[3]/2)
#'
#' AABB_max <- c(AABB[1, 1] + edge_length[1]/2,
#'               AABB[1, 2] + edge_length[2]/2,
#'               AABB[1, 3] + edge_length[3]/2)
#'
#' #Plot
#' cube <- rgl::cube3d()
#' cube <- rgl::scale3d(cube, edge_length[1]/2,
#'                            edge_length[2]/2,
#'                            edge_length[3]/2)
#' box <- rgl::translate3d(cube, AABB[1, 1], AABB[1, 2], AABB[1, 3])
#' rgl::shade3d(box, col= "green", alpha = 0.6)
#' rgl::points3d(orig, size = 4, col = "black")
#' rgl::points3d(end, size = 4, col = "red")
#'
#' #Line no intercepted
#' rgl::lines3d(c(orig[1, 1], end[1, 1]),
#'              c(orig[1, 2], end[1, 2]),
#'              c(orig[1, 3], end[1, 3]), col = "grey")
#'
#' line_AABB(orig[1,], end[1,], AABB_min, AABB_max)
#'
#' #Both ends falls inside
#' rgl::lines3d(c(orig[2, 1], end[2, 1]),
#'              c(orig[2, 2], end[2, 2]),
#'              c(orig[2, 3], end[2, 3]), col = "red")
#'
#' line_AABB(orig[2,], end[2,], AABB_min, AABB_max)
#'
#' #Oring falls inside, but not the end.
#' rgl::lines3d(c(orig[3, 1], end[3, 1]),
#'              c(orig[3, 2], end[3, 2]),
#'              c(orig[3, 3], end[3, 3]), col = "blue")
#'
#' line_AABB(orig[3,], end[3,], AABB_min, AABB_max)
#'
#' #End falls inside, but not the orig
#' rgl::lines3d(c(orig[4, 1], end[4, 1]),
#'              c(orig[4, 2], end[4, 2]),
#'              c(orig[4, 3], end[4, 3]), col = "green")
#'
#' line_AABB(orig[4,], end[4,], AABB_min, AABB_max)
#'
#' #Some segments of the line are intercepted
#' rgl::lines3d(c(orig[5, 1], end[5, 1]),
#'              c(orig[5, 2], end[5, 2]),
#'              c(orig[5, 3], end[5, 3]), col = "black")
#'
#' line_AABB(orig[5,], end[5,], AABB_min, AABB_max)
#'
#'
#' @export
line_AABB <- function(orig, end, AABB_min, AABB_max) {

  results <- as.vector(line_AABB_rcpp(as.matrix(orig), as.matrix(end), AABB_min, AABB_max))
  names(results) <- c("code", "length")

  return(results)
}

