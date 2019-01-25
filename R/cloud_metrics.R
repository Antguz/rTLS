#' @title Point cloud metrics
#'
#' @description Estimate different metrics on the poits of a cloud. It estimate 10 parameters based on Wang et al. 2017.
#'
#' @param cloud A \code{data.table} with xyz coordinates in the first three columns or an object of class \code{neighborhood}.
#' @param cloud_b A \code{data.table} with xyz coordinates in the first three columns. If \code{cloud_b} is \code{NULL}, \code{cloud_b == cloud}. \code{NULL} as default.
#' @param basic Logical, if \code{TRUE} it estimate basic metrics. \code{basic = TRUE} as default.
#' @param distribution Logical, if \code{TRUE} it estimate distribution metrics of points. \code{distribution = TRUE} as default.
#' @param dimensionality Logical, if \code{TRUE} it estimate dimensionality metrics. \code{dimensionality = TRUE} as default.
#' @param method A character string specifying the method to estimated the neighbors. It most be one of \code{"sphere"} or \code{"knn"}.
#' @param radius A \code{numeric} vector of a length 1 representing the radius of the sphere to consider. This need be used if \code{method = "sphere"} and this may used if \code{method = "knn"}.
#' @param k An integer of a length 1 representing the number of neighbors to consider. This need be used if \code{method = "knn"}.
#' @param parallel Logical, if \code{TRUE} it use a parallel processing.
#' @param threads An \code{integer} >= 0 describing the number of CPU use. This need to be used if  \code{parallel = TRUE}
#'
#' @return A \code{data.frame} with the estimated parameters.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @seealso basic.metrics, distribution, dimensionality
#'
#' @references Wang, D., Hollaus, M., & Pfeifer, N. (2017). Feasibility of machine learning methods for separating wood and leaf points from Terrestrial Laser Scanning data. ISPRS Annals of Photogrammetry, Remote Sensing & Spatial Information Sciences, 4.
#'
#' @examples
#' data("pc_tree")
#'
#' ###Run on the first 100 points of a point cloud of class data.table using a sphere method with a radius of 0.2
#' cloud_metrics(pc_tree[1:100,], pc_tree, method = "sphere", radius = 0.2)
#'
#' ###Run on the first 100 points of a point cloud of class data.table using a knn method with a k = 10.
#' cloud_metrics(pc_tree[1:100,], pc_tree, method = "knn", k = 10)
#'
#'
#' ###Estimate neighborhood on first 100 points using a sphere method and a radius of 0.2 and then calculate their metrics
#' dist <- neighborhood(pc_tree[1:100,], pc_tree, method = "sphere", radius = 0.2)
#' cloud_metrics(dist)
#'
#'@export
cloud_metrics <- function(cloud, cloud_b = NULL, basic = TRUE, distribution = TRUE, dimensionality = TRUE, method = NULL, radius = NULL, k, n_replicates = NULL, parallel = FALSE, threads = NULL) {

  if(parallel == TRUE) { #Select parameters to make the method parallel.
    if(is.null(threads) == TRUE) {
      stop("Include the number of threads to use")
    }
    setDTthreads(threads = threads, restore_after_fork = FALSE)
    print(paste("", "Threads to use: ", getDTthreads(), sep = ""))
  }

  if(class(cloud)[1] == "neighborhood") {  ####For a object of neighborhood
    final <- cloud$cloud
    neig <- cloud$neighborhood
    radius <- ifelse(names(cloud$parameter) == "radius", cloud$parameter, NULL)

    if(basic == TRUE) {
      print("Calculating basic metrics")
      pb <- txtProgressBar(min = 0, max = nrow(final), style = 3)
      basc <- neig[, {setTxtProgressBar(pb, .GRP) ; basic_metrics(.SD, radius = radius)}, by = points]
      final <- cbind(final, basc[ , c(2:5)])
    }

    if(distribution == TRUE) {
      print("Calculating distribution metrics")
      pb <- txtProgressBar(min = 0, max = nrow(final), style = 3)
      disp <- neig[, {setTxtProgressBar(pb, .GRP) ; distribution(.SD, radius = radius, n_replicates = n_replicates)}, by = points]
      final <- cbind(final, disp[ , c(2:5)])
    }

    if(dimensionality == TRUE) {
      print("Calculating dimensionality metrics")
      pb <- txtProgressBar(min = 0, max = nrow(final), style = 3)
      dimen <- neig[, {setTxtProgressBar(pb, .GRP) ; dimensionality(.SD)}, by = points]
      final <- cbind(final, dimen[ , c(2:10)])
    }

  } else if(class(cloud)[1] != "neighborhood") { ###For a object of class data.frame

    if(method == "sphere") {  ###If method is sphere
      print("calculating spheres and their metrics")
      pb <- txtProgressBar(min = 0, max = nrow(cloud), style = 3)
      final <- cloud[, {setTxtProgressBar(pb, .GRP) ; sphere_metrics(.SD, cloud_b, radius, basic, distribution, dimensionality, n_replicates)}, by = seq_len(nrow(cloud))]
      final <- final[, 2:ncol(final)]

    } else if(method == "knn") { ###If method is knn
      print("calculating knn and their metrics")
      pb <- txtProgressBar(min = 0, max = nrow(cloud), style = 3)
      final <- cloud[, {setTxtProgressBar(pb, .GRP) ; knn_metrics(.SD, cloud_b, k, radius, basic, distribution, dimensionality, n_replicates)}, by = seq_len(nrow(cloud))]
      final <- final[, 2:ncol(final)]
    }
  }

  if(parallel == TRUE) { #Stop threads
    setDTthreads(threads = 1, restore_after_fork = TRUE)
  }
  return(final)
}
