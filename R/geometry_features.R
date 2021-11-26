#' @title Geometry features of Neighboring Points.
#'
#' @description Estimate geometry features of neighboring points in a cloud.
#'
#' @param cloud A \code{data.table} with *XYZ* coordinates in the first three columns.
#' @param method A character string specifying the method to estimate the neighbors. It most be one of \code{"radius_search"} or \code{"knn"}.
#' @param radius A \code{numeric} vector representing the radius for search to consider. This needs be used if \code{method = "radius_search"}.
#' @param k An \code{integer} vector representing the number of neighbors to consider. This needs be used if \code{method = "knn"}.
#' @param max_neighbour An \code{integer} specifying the maximum number of points to look around each query point for a given radius. This needs be used if \code{method = "radius_search"}.
#' @param distance Type of distance to calculate. \code{"euclidean"} as default. Look \code{hnsw_knn} for more options.
#' @param target Logic. If \code{TRUE}, it consider the each target point for the calculations of geometry features.
#' @param threads An \code{integer} specifying the number of threads to use for parallel processing. Experiment to see what works best for your data on your hardware.
#' @param verbose If \code{TRUE}, log messages to the console.
#' @param progress If \code{TRUE}, log a progress bar when \code{verbose = TRUE}. Tracking progress could cause a small overhead.
#' @param ... Arguments passed to \code{hnsw_build} and \code{hnsw_search}.
#'
#'
#' @details The function returns the geometry features of the neighboring points
#' of a given point in \code{cloud}. Geometry features are represented by the
#' relative values of the eigenvalues derived from a covariance matrix of the
#' neighboring points. Geometry features are not estimated on target points
#' with less than 3 neighboring points.
#'
#'
#' @return A \code{array} describing the point of the \code{cloud} in rows,
#' the relative eigenvalues in columns, and the \code{radius} or \code{k} per slide.
#' If \code{method = "radius_search"}, it add in the first column the number of
#' neighboring points.
#'
#' @author J. Antonio Guzmán Q.
#'
#'
#' @examples
#' #Create cloud
#' example <- data.table(X = runif(200, min=0, max=10),
#'                       Y = runif(200, min=0, max=10),
#'                       Z = runif(200, min=0, max=10))
#'
#'
#' #Using knn method with two different k
#' k_test <- c(5, 10)
#' geometry_features(example, method = "knn", k = k_test)
#'
#' #Using radius search method with two different radius
#' radius_test <- c(3, 4)
#' geometry_features(example, method = "radius_search", radius = radius_test, max_neighbour = 200)
#'
#' @export
geometry_features <- function(cloud, method, radius, k, max_neighbour, distance = "euclidean", target = FALSE, threads = 1L, verbose = FALSE, progress = TRUE, ...) {

  dist <- match.arg(distance, c("l2", "euclidean", "cosine", "ip"))

  if(method == "radius_search") {

    if(max_neighbour > nrow(cloud)) {
      stop("max_neighbour value can not be greater than nrow(cloud)")
    }

    #Distances and radius
    dist <- match.arg(distance, c("l2", "euclidean", "cosine", "ip"))
    radius_max <- max(radius)

    #Get index
    index <- radius_search(query = as.matrix(cloud),
                           ref = as.matrix(cloud),
                           radius = radius_max,
                           max_neighbour = max_neighbour,
                           distance = dist,
                           same = target,
                           threads = threads,
                           verbose = verbose,
                           progress = progress)

    index[, 1] <- index[, 1] - 1
    index[, 2] <- index[, 2] - 1

    #Estimate features
    results <- features_radius_rcpp(index = as.matrix(index),
                                    query = as.matrix(cloud),
                                    radius = radius,
                                    threads = threads,
                                    progress = progress,
                                    ...)

    col_names <- c("npoints", "eig1", "eig2", "eig3")
    lev_names <- paste0("radius_", radius)

    results <- provideDimnames(results,
                               base = list(as.character(seq_along(1:nrow(cloud))), col_names, lev_names))

  } else if(method == "knn") {

    #Add and extra if does not count the target point
    if(target == TRUE) {
      k_value <- k + 1
    } else {
      k_value <- k
    }

    if(min(k_value) <= 3) {
      stop("k values must be greater than three")
    }

    if(max(k_value) > nrow(cloud)) {
      stop("k values can not be greater than nrow(cloud)")
    }

    k_max <- max(k_value)

    #Estimate index

    index <- knn(query = as.matrix(cloud),
                 ref = as.matrix(cloud),
                 k = k_max,
                 distance = dist,
                 same = target,
                 threads = threads,
                 verbose = verbose,
                 progress = progress,
                 ...)

    index[, 1] <- index[, 1] - 1
    index[, 2] <- index[, 2] - 1
    index <- index[,1:3]

    #Estimate features
    results <- features_knn_rcpp(index = as.matrix(index),
                                 query = as.matrix(cloud),
                                 k = k_value,
                                 threads = threads,
                                 progress = progress)

    #Provide names
    col_names <- c("eig1", "eig2", "eig3")
    lev_names <- paste0("k_", k)
    results <- provideDimnames(results,
                               base = list(as.character(seq_along(1:nrow(cloud))), col_names, lev_names))

  }

  return(results)
}
