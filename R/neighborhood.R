#' @title Neighboring points
#'
#' @description Estimate neighboring points based on two methods.
#'
#' @param cloud A \code{data.table} with xyz coordinates in the first three columns.
#' @param cloud_b A \code{data.table} with xyz coordinates in the first three columns. If \code{cloud_b == NULL}, \code{cloud_b == cloud}. \code{NULL} as default.
#' @param method A character string specifying the method to estimated the neighbors. It most be one of  \code{"sphere"} or  \code{"knn"}.
#' @param radius An integer of a length 1 representing the number of neighbors to consider. This need to be used if  \code{method = "sphere"}, and this may usted if \code{method = "knn"}.
#' @param k An integer of a length 1 representing the number of neighbors to consider. This need to be used if \code{method = "knn"}.
#' @param parallel Logical, if \code{TRUE} it use a parallel processing. \code{FALSE} as default.
#' @param threads An \code{integer} >= 0 describing the number of CPU use. This need to be used if  \code{parallel = TRUE}
#'
#' @return An object of class \code{neighborhood} which is a nested list that describe the cloud point used (\code{cloud}), the parameter \code{radius} or \code{k} used, and the resulting neighbor points per point (\code{neigborhood}).
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#' @examples
#' data("pc_tree")
#'
#' ###Estimate the niegborhood based in a sphere of a radius 0.2
#' neighborhood(pc_tree[50:100,] , pc_tree, method = "sphere", radius = 0.2)
#'
#' ###Estimate the niegborhood based on knn
#' neighborhood(pc_tree, method = "knn", k = 10)
#'
#' ###Parallel TRUE with 4 threads
#' neighborhood(pc_tree, method = "sphere", radius = 0.2, parallel = TRUE, threads = 4)
#'
#' @export
neighborhood <- function(cloud, cloud_b = NULL, method, radius, k, parallel = FALSE, threads = NULL) {

  if(is.null(cloud_b) == TRUE) { #Selecting the cloud to calculated the neighborhood
    cloud_b <- cloud
  }

  cloud <- cloud[, 1:3] #Specify parameters as inputs.
  cloud_b <- cloud_b[, 1:3]

  colnames(cloud) <- c("X", "Y", "Z") #Change names of columns
  colnames(cloud_b) <- c("X", "Y", "Z")

  if(parallel == TRUE) { #Select parameters to make the method parallel.
    if(is.null(threads) == TRUE) {
      stop("Include the number of threads to use")
    }
    setDTthreads(threads = threads, restore_after_fork = TRUE)
    print(paste("", "Threads to use: ", getDTthreads(), sep = ""))
  }

  #Calculating neighborhood
  print("Calculating neighbors")
  pb <- txtProgressBar(min = 0, max = nrow(cloud), style = 3) #Set progress bar

  if(method == "sphere") {  #Method sphere
    results <- cloud[, {setTxtProgressBar(pb, .GRP) ; sphere_neighbors(.SD, cloud_b, radius)}, by = seq_len(nrow(cloud))]
    colnames(results) <- c("points", "X", "Y", "Z", "distance")
    parameter <- radius
    names(parameter) <- "radius"

  } else if(method == "knn") { #Method knn
    results <- cloud[, {setTxtProgressBar(pb, .GRP) ; knn_neighbors(.SD, cloud_b, k, radius)}, by = seq_len(nrow(cloud))]
    colnames(results) <- c("points", "X", "Y", "Z", "distance")
    parameter <- radius
    names(parameter) <- "radius"

  }

  final <- list(cloud = cloud, parameter = parameter, neighborhood = results)

  class(final) <- "neighborhood"
  return(final)
}
