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
#' @param cores An \code{integer} >= 0 describing the number of cores use. This need to be used if  \code{parallel = TRUE}
#'
#' @return An object of class \code{neighborhood} which is a nested list that describe the cloud point used (\code{cloud}), the parameter \code{radius} or \code{k} used, and the resulting neighbor points per point (\code{neigborhood}).
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#' @examples
#' data("pc_tree")
#'
#' ###Estimate the niegborhood based in a sphere of a radius 0.2
#' neighborhood(pc_tree, method = "sphere", radius = 0.2)
#'
#' ###Estimate the niegborhood based on knn
#' neighborhood(pc_tree, method = "knn", k = 10)
#'
#' ###Parallel TRUE with 4 cores
#' neighborhood(pc_tree, method = "sphere", radius = 0.2, parallel = TRUE, cores = 4)
#'
#' @export
neighborhood <- function(cloud, cloud_b = NULL, method, radius, k, parallel = FALSE, cores = NULL) {

  if(is.null(cloud_b) == TRUE) { #Selecting the cloud to calculated the neighborhood
    cloud_b <- cloud
  }

  cloud <- cloud[, 1:3] #Specify parameters as inputs.
  cloud_b <- cloud_b[, 1:3]

  colnames(cloud) <- c("X", "Y", "Z") #Change names of columns
  colnames(cloud_b) <- c("X", "Y", "Z")

  if(parallel == FALSE) {

    if(method == "sphere") {  #Method sphere

      print("Calculating spheres around points")
      pb <- txtProgressBar(min = 0, max = nrow(cloud), style = 3) #Set progress bar

      results <- cloud[, {setTxtProgressBar(pb, .GRP) ; sphere_neighbors(.SD, cloud_b, radius)}, by = seq_len(nrow(cloud))]
      colnames(results) <- c("points", "X", "Y", "Z", "distance")
      parameter <- radius
      names(parameter) <- "radius"

    } else if(method == "knn") { #Method knn

      print("Calculating knn")
      pb <- txtProgressBar(min = 0, max = nrow(cloud), style = 3) #Set progress bar

      results <- cloud[, {setTxtProgressBar(pb, .GRP) ; knn_neighbors(.SD, cloud_b, k, radius)}, by = seq_len(nrow(cloud))]
      colnames(results) <- c("points", "X", "Y", "Z", "distance")
      parameter <- k
      names(parameter) <- "k"

    }

  } else if(parallel == TRUE) {

    if(is.null(cores) == TRUE) {
      stop("Select the number of cores")
    }

    cl <- makeCluster(cores, outfile="")
    registerDoSNOW(cl)

    if(method == "sphere") {  #Method sphere

      print("Calculating spheres around points")
      pb <- txtProgressBar(min = 0, max = nrow(cloud), style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress=progress)

      results <- foreach(i = 1:nrow(cloud), .inorder = FALSE, .combine= rbind, .packages = c("data.table", "rTLS"), .options.snow = opts) %dopar% {
        results <- sphere_neighbors(x = cloud[i,], cloud = cloud_b, radius = radius)
        results$points <- i
        return(results[, c(5, 1:4)])
      }

      results <- results[order(points)]
      parameter <- radius
      names(parameter) <- "radius"

    } else if(method == "knn") { #Method knn

      print("Calculating knn")
      pb <- txtProgressBar(min = 0, max = nrow(cloud), style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress=progress)

      results <- foreach(i = 1:nrow(cloud), .inorder = FALSE, .combine= rbind, .packages = c("data.table", "rTLS"), .options.snow = opts) %dopar% {
        results <- knn_neighbors(x = cloud[i,], cloud = cloud_b, k= k, radius = radius)
        results$points <- i
        return(results[, c(5, 1:4)])
      }

      results <- results[order(points)]
      parameter <- k
      names(parameter) <- "k"

    }
    close(pb)
    stopCluster(cl)
  }

  final <- list(cloud = cloud, parameter = parameter, neighborhood = results)

  class(final) <- "neighborhood"
  return(final)
}
