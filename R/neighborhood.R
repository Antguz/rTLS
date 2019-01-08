#' @import dplyr
#'
#' @title Neighboring points
#'
#' @description Estimate neighboring points based on two methods.
#'
#' @param cloud A \code{matrix} or \code{data.frame} with xyz coordinates in the first three columns.
#' @param cloud_b A \code{matrix} or \code{data.frame} with xyz coordinates in the first three columns. If \code{cloud_b == NULL}, \code{cloud_b == cloud}. \code{NULL} as default.
#' @param method A character string specifying the method to estimated the neighbors. It most be one of  \code{"distance"} or  \code{"knn"}.
#' @param radius An integer of a length 1 representing the number of neighbors to consider. This will be used if  \code{method = "distance"}.
#' @param k An integer of a length 1 representing the number of neighbors to consider. This will be used if  \code{method = "knn"}.
#' @param parallel Logical, if \code{TRUE} it use a parallel processing. \code{FALSE} as default.
#'
#' @return An object of class \code{neighborhood} which is a nested list that describe the cloud point used (\code{cloud.used}), the parameter \code{radius} or \code{k} used, and the resulting neighbor points per point (\code{neigborhood}).
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#' @examples
#' data("pc_tree")
#'
#' ###Estimate the niegborhood based on distance
#' neighborhood(pc_tree, method = "distance", radius = 0.2, parallel = FALSE)
#'
#' ###Estimate the niegborhood based on knn
#' neighborhood(pc_tree, method = "knn", k = 10, parallel = FALSE)
#'
#' ###Parallel TRUE
#' require(doParallel)
#' detectCores() ### Number of cores of your computer
#' cores <- makeCluster(4) ### Set number of cores to work
#' registerDoParallel(cores)
#'
#' neighborhood(pc_tree, method = "distance", radius = 0.2, parallel = TRUE)
#'
#' stopCluster(cores)
#'
#' @export
neighborhood <- function(cloud, cloud_b = NULL, method, radius, k, parallel = NULL) {

  par <- ifelse(is.null(parallel) == TRUE, FALSE, ifelse(parallel == FALSE, FALSE, TRUE))

  if(is.null(cloud_b) == TRUE) {

    if(method == "distance") {  #Method distance
      pack <- list(.packages = c("dplyr", "bio3d"))
      results <- alply(cloud, .margins = 1, .fun = dist_neighbors, cloud = cloud, radius = radius, .progress = "text", .parallel = par, .paropts = pack, .inform = FALSE)
      parameter <- radius
    } else if(method == "knn") { #Method knn
      pack <- list(.packages = c("dplyr", "nabor"))
      results <- alply(cloud, .margins = 1, .fun = knn_neighbors, cloud = cloud, k = k, .progress = "text", .parallel = par, .paropts = pack, .inform = FALSE)
      parameter <- k
    }
  } else if(is.null(cloud_b) == FALSE) {

    if(method == "distance") {  #Method distance
      pack <- list(.packages = c("dplyr", "bio3d"))
      results <- alply(cloud, .margins = 1, .fun = dist_neighbors, cloud = cloud_b, radius = radius, .progress = "text", .parallel = par, .paropts = pack, .inform = FALSE)
      parameter <- radius
    } else if(method == "knn") { #Method knn
      pack <- list(.packages = c("dplyr", "nabor"))
      results <- alply(cloud, .margins = 1, .fun = knn_neighbors, cloud = cloud_b, k = k, .progress = "text", .parallel = par, .paropts = pack, .inform = FALSE)
      parameter <- k
    }
  }

  results <- llply(results,
             .fun = function(x) {x <- x[order(x[,4]),]},
             .inform = FALSE,
             .parallel = FALSE)

  final <- list(cloud.used = cloud, parameter = parameter, niegborhood = results)

  class(final) <- "neighborhood"
  return(final)
}
