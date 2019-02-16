#' @title Point cloud filter
#'
#' @description Perform a different filtering methods on a point cloud.
#'
#' @param cloud A \code{data.table} with xyz coordinates in the first three columns.
#' @param method A character string specifying the method to estimated the neighbors. It most be one of \code{"sor"} for Statistical Outlier Removal or \code{"mnp"} for Minimun number of points.
#' @param k An integer of a length 1 representing the number of neighboring points to consider. This need to be used if \code{method = "sor"}.
#' @param radius A \code{numeric} vector of a length 1 representing the distance to consider for the extraction of number of points. This need to be used if \code{method = "mnp"} and this could be used if \code{method = "sor"}.
#' @param nSigma  A \code{numeric} vector of a length 1 representing the standard deviation multiplier.
#' @param parallel Logical, if \code{TRUE} it use a parallel processing. \code{FALSE} as default.
#' @param plot Logical, if \code{TRUE} returns a plot showing the parameters used for filtering.
#'
#' @return A filtered \code{data.table}.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#' @references Wang, D., Hollaus, M., & Pfeifer, N. (2017). Feasibility of machine learning methods for separating wood and leaf points from Terrestrial Laser Scanning data. ISPRS Annals of Photogrammetry, Remote Sensing & Spatial Information Sciences, 4.
#'
#' @examples
#' data("pc_tree")
#'
#' ###Filter a cloud using the SOR method with a k of 20 and nSigma of 2
#' filter(pc_tree, method = "sor", k = 20, nSigma = 2, parallel = TRUE, cores = 8)
#'
#' ###Filter a cloud using the MNP method with a radius of 0.5 and mnp of 5.
#' filter(pc_tree, method = "mnp", radius = 0.5, mnp = 100, parallel = TRUE, cores = 8)
#'
#' @export
filter <- function(cloud, method, k, radius = NULL, nSigma, mnp, parallel = FALSE, cores = NULL, plot = TRUE) {

  if(method == "sor") {

    neigh <- neighborhood(cloud, method = "knn", radius = radius, k = k, parallel = parallel, cores = cores)

    distances <- aggregate(neigh$neighborhood[, 5], list(neigh$neighborhood$points), mean)
    max.distance <- mean(distances$distance) + (nSigma * sd(distances$distance))
    keep <- distances$distance <= max.distance

    final <- cloud[keep,]

    print(paste("", length(keep[keep == FALSE]), " points of ", nrow(cloud), " were excluded", sep = ""))

  } else if(method == "mnp") {

    if(is.null(radius) == TRUE) {
      stop("radius of the spheres to consider is missing")
    }

    neigh <- neighborhood(cloud, method = "sphere", radius = radius, parallel = parallel, cores = cores)

    points <- neigh$neighborhood[, .N, by = points]
    keep <- points$N >= mnp

    final <- cloud[keep,]

    print(paste("", length(keep[keep == FALSE]), " points of ", nrow(cloud), " were excluded", sep = ""))
  }
  return(final)
}
