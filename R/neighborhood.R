#' @title Neighboring Points
#'
#' @description Estimates neighboring points based on two methods: sphere and knn.
#'
#' @param cloud A \code{data.table} with three columns representing the *XYZ* coordinates of a point cloud.
#' @param cloud_b A \code{data.table} with three columns representing the *XYZ* coordinates of a point cloud to extract the neighboring points. If \code{NULL}, \code{cloud_b == cloud}. \code{NULL} as default.
#' @param method A \code{character} string specifying the method to estimated the neighbors. It most be one of  \code{"sphere"} or  \code{"knn"}.
#' @param radius An \code{numeric} vector of a length 1 representing the number of neighbors to consider. This need to be used if  \code{method = "sphere"}, and this may usted if \code{method = "knn"}.
#' @param k An \code{integer} of a length 1 representing the number of neighbors to consider. This need to be used if \code{method = "knn"}.
#' @param parallel Logical, if \code{TRUE} it use a parallel processing. \code{FALSE} as default.
#' @param cores An \code{integer >= 0}  describing the number of cores use. This need to be used if \code{parallel = TRUE}.
#'
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom data.table .GRP
#' @importFrom data.table .SD
#'
#' @return An object of class \code{neighborhood} which is a nested list that describes the cloud point used (\code{cloud}), the parameter \code{radius} or \code{k} used, and the resulting neighbor points per point (\code{neighborhood}).
#' @author J. Antonio Guzmán Q.
#'
#' @seealso \code{\link{knn_neighbors}}, \code{\link{sphere_neighbors}}, \code{\link{cloud_metrics}}
#'
#' @examples
#' data(pc_tree)
#'
#' ###Apply funtion on 1000 random rows of a point cloud
#' cloud.random <- pc_tree[sample(nrow(pc_tree), 1000), ]
#'
#' ##Without using parallel processing
#' #Estimate the niegborhood based in the sphere method using a radius of 0.2.
#' neighborhood(cloud.random, pc_tree, method = "sphere", radius = 0.2)
#'
#' #Estimate the niegborhood based on the knn method using a k = 10.
#' neighborhood(cloud.random, pc_tree, method = "knn", k = 10)
#'
#' #Estimate the niegborhood based on the knn method using a k = 10 and a fixed radius of 0.5.
#' neighborhood(cloud.random, pc_tree, method = "knn", k = 10, radius = 0.2)
#'
#' \dontrun{
#' #Parallel TRUE with 4 cores
#' neighborhood(cloud.random, pc_tree, method = "sphere", radius = 0.2, parallel = TRUE, cores = 4)
#' }
#'
#' @export
neighborhood <- function(cloud, cloud_b = NULL, method, radius = NULL, k, parallel = FALSE, cores = NULL) {

  if(is.null(cloud_b) == TRUE) { #Selecting the cloud to calculated the neighborhood
    cloud_b <- cloud
  }

  cloud <- cloud[, 1:3] #Specify parameters as inputs.
  cloud_b <- cloud_b[, 1:3]

  colnames(cloud) <- c("X", "Y", "Z") #Change names of columns
  colnames(cloud_b) <- c("X", "Y", "Z")

  if(parallel == FALSE) {

    if(method == "sphere") {  #Method sphere

      if(is.null(radius) == TRUE) {
        stop("Argument radius is missing")
      }

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

      if(is.null(radius) == TRUE) {
        stop("argument radius is missing")
      }

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
