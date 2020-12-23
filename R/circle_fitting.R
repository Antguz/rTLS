#' Adaptive RANSAC Circle Fitting
#'
#' Adaptive random sample consensus for cicle fitting.
#'
#' @param cloud A \code{data.table} with *XY* coordinates in the first two columns.
#' @param fpoints A \code{numeric} vector between 0 and 1 representing the fraction of point samples that will be used during each iteration.
#' @param pconf A \code{numeric} vector between 0 and 1 describing the confidence threshold to consider a point in a given fitted circle outlier or inlier.
#' @param poutlier A \code{numeric} vector of length two describing the proportion of outliers to consider inside or outsite of the \code{pconf} threshold.
#' @param max_iterations An \code{integer} specifying the number of iterations. If \code{NULL}, the number of iterations are automaticaly estimated using \code{pconf}, \code{1 - poutlier}, and  \code{1 - fpoints}; see details.
#' @param threads An \code{integer} specifying the number of threads to use for parallel processing. Experiment to see what works best for your data on your hardware.
#' @param plot Logical. If \code{TRUE}, it provides visual representation of the fitted circle.
#'
#' @return A \code{data.table} with the *XY* coordinate information of the circle center, the radius, the error based on the least squares fit, and the proportion of inliers.
#' @author J. Antonio Guzmán Q.
#'
#' @importFrom data.table data.table
#' @importFrom data.table fread
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom grDevices chull
#' @importFrom graphics points
#' @useDynLib rTLS, .registration = TRUE
#'
#' @seealso \code{\link{voxels_counting}}
#'
#' @examples
#' #' #Import an example point cloud
#' path <- system.file("extdata", "pc_tree.txt", package = "rTLS")
#'
#' #Creates a stand of 4 trees with 10% of overlap
#' files <- rep(path, 4)
#' artificial_stand(files, n.trees = 4, dimension = c(15, 15), overlap = 10)
#'
#' #Creates a stand of 4 trees with their locations
#' location <- data.table(X = c(5, 10, 10, 5), Y = c(5, 5, 10, 10))
#' artificial_stand(files, n.trees = 4, dimension = c(15, 15), coordinates = location)
#'
#'
#' @export
circle_fitting

circleRANSAC_rcpp <- function(cloud, fract_points, pconf, poutlier, max_iterations, threads = 1L)
