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
#' @importFrom stats qnorm
#' @importFrom graphics lines
#' @importFrom graphics points
#'
#' @seealso \code{\link{tree_metrics}}, \code{\link{trunk_volume}}
#'
#' @examples
#'
#' #Point cloud
#' data("pc_tree")
#'
#' #Subset region at at breast height
#' sub <- pc_tree[between(Z, 1.25, 1.35),]
#'
#' #Fit circle
#' circleRANSAC(sub, fpoints = 0.2, pconf = 0.95, poutlier = c(0.5, 0.5), max_iterations = 100)
#'
#' @export
circleRANSAC <- function(cloud, fpoints, pconf, poutlier, max_iterations, threads = 1L, plot = TRUE) {

  z_value <- qnorm(pconf)

  circle <- try(circleRANSAC_rcpp(as.matrix(cloud), fpoints, z_value, poutlier, max_iterations, threads = threads), silent = TRUE)

  if(class(circle)[1] == "try-error") {
    stop("With the defined parameters it is not possible to reach a solution. Increase the poutlier or reduce pconf")
  }

  circle <- as.data.table(circle)
  circle <- circle[1,]

  colnames(circle) <- c("X", "Y", "radius", "RMSE")

  if(plot == TRUE) {

    angles <- seq(0,360, by=1)
    angles <- angles*pi/180
    xv <- cos(angles)*circle[[3]]+circle[[1]]
    yv <- sin(angles)*circle[[3]]+circle[[2]]

    sub_title <- paste0("X = ", round(circle[[1]], 3),
                        "; Y= ", round(circle[[2]], 3),
                        "; radius = ", round(circle[[3]], 3))

    plot(cloud[,1:2], xlab = "X", ylab = "Y", sub = sub_title)
    points(circle[,1], circle[,2], col = "red")
    lines(xv, yv, col = "red")
  }

  return(circle)
}


