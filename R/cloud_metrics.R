#' @title Point cloud metrics
#'
#' @description Estimate different metrics on the poits of a cloud.
#'
#' @param cloud A \code{data.table} with *XYZ* coordinates in the first three columns or an object of class \code{"neighborhood"}.
#' @param cloud_b A \code{data.table} with *XYZ* coordinates in the first three columns. If \code{cloud_b} is \code{NULL}, \code{cloud_b == cloud}. \code{NULL} as default.
#' @param basic Logical, if \code{TRUE} it estimates \code{\link{basic_metrics}}. \code{TRUE} as default.
#' @param distribution Logical, if \code{TRUE} it estimates \code{\link{distribution}} metrics of points. \code{TRUE} as default.
#' @param dimensionality Logical, if \code{TRUE} it estimates \code{\link{dimensionality}} metrics. \code{TRUE} as default.
#' @param method A character string specifying the method to estimate the neighbors. It most be one of \code{"sphere"} or \code{"knn"}.
#' @param radius A \code{numeric} vector of a length 1 representing the radius of the sphere to consider. This needs be used if \code{method = "sphere"} and may used if \code{method = "knn"}.
#' @param k An integer of a length 1 representing the number of neighbors to consider. This needs be used if \code{method = "knn"}.
#' @param n_replicates An \code{interger} of a length 1 representing the number of replicates to estimate the expected distance. This needs to be used if \code{distribution = TRUE}.If \code{NULL}, it uses the number of neighbors per point. \code{NULL} as default.
#' @param parallel Logical, if \code{TRUE} it uses parallel processing.
#' @param cores An \code{integer} >= 0 describing the number of cores use. This needs to be used if  \code{parallel = TRUE}.
#'
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom data.table .GRP
#' @importFrom data.table .SD
#'
#' @return A \code{data.frame} with the estimated parameters.
#' @author J. Antonio Guzmán Q. and Ronny Hernandez
#'
#' @seealso \code{\link{basic_metrics}}, \code{\link{distribution}}, \code{\link{dimensionality}}, \code{\link{neighborhood}}
#'
#' @examples
#' data("pc_tree")
#'
#' ###On objects of class neighborhood
#' ##Calculate the neighborhood of 1000 random rows of a point cloud using the sphere method and a radius of 0.2.
#' cloud.random <- pc_tree[sample(nrow(pc_tree), 1000), ]
#' dist <- neighborhood(cloud.random, pc_tree, method = "sphere", radius = 0.2)
#'
#' #Estimate metrics without using parallel.
#' cloud_metrics(dist)
#'
#' \dontrun{
#' #Estimate matrics using parallel processing with 4 cores.
#' cloud_metrics(dist, parallel = TRUE, cores = 4)
#' }
#'
#' ###On objects of class data.table.
#' ##Select 1000 random rows of a point cloud.
#' cloud.random <- pc_tree[sample(nrow(pc_tree), 1000), ]
#'
#' #Estimate metrics without using parallel by creating spheres of a radius of 0.2.
#' cloud_metrics(cloud.random, pc_tree, method = "sphere", radius = 0.2)
#'
#' \dontrun{
#' #Estimate metrics metrics in parallel using 4 cores by creating spheres of a radius of 0.2.
#' cloud_metrics(cloud.random, pc_tree, method = "sphere", radius = 0.2, parallel = TRUE, cores = 4)
#' }
#'
#'@export
cloud_metrics <- function(cloud, cloud_b = NULL, basic = TRUE, distribution = TRUE, dimensionality = TRUE, method = NULL, radius = NULL, k, n_replicates = NULL, parallel = FALSE, cores) {

  if(parallel == FALSE) { ###If parallel is false-----------------------------------------------------------------------

    if(class(cloud)[1] == "neighborhood") {  ####For a object of neighborhood
      final <- cloud$cloud

      if(names(cloud$parameter) == "radius") {
        radius <- cloud$parameter
      } else {
        radius <- NULL
      }

      if(basic == TRUE) {
        print("Calculating basic metrics")
        pb <- txtProgressBar(min = 0, max = nrow(final), style = 3)
        basc <- cloud$neighborhood[, {setTxtProgressBar(pb, .GRP) ; basic_metrics(.SD, radius = radius)}, by = points]
        final <- cbind(final, basc[ , c(2:5)])
      }

      if(distribution == TRUE) {
        print("Calculating distribution metrics")
        pb <- txtProgressBar(min = 0, max = nrow(final), style = 3)
        disp <- cloud$neighborhood[, {setTxtProgressBar(pb, .GRP) ; distribution(.SD, radius = radius, n_replicates = n_replicates)}, by = points]
        final <- cbind(final, disp[ , c(2:5)])
      }

      if(dimensionality == TRUE) {
        print("Calculating dimensionality metrics")
        pb <- txtProgressBar(min = 0, max = nrow(final), style = 3)
        dimen <- cloud$neighborhood[, {setTxtProgressBar(pb, .GRP) ; dimensionality(.SD)}, by = points]
        final <- cbind(final, dimen[ , c(2:10)])
      }

    } else if(class(cloud)[1] != "neighborhood") { ###For a object of class data.table

      if(method == "sphere") {  ###If method is sphere
        print("Calculating spheres and their metrics")
        pb <- txtProgressBar(min = 0, max = nrow(cloud), style = 3)
        final <- cloud[, {setTxtProgressBar(pb, .GRP) ; sphere_metrics(.SD, cloud_b, radius, basic, distribution, dimensionality, n_replicates)}, by = seq_len(nrow(cloud))]
        final <- final[, 2:ncol(final)]

      } else if(method == "knn") { ###If method is knn
        print("Calculating knn and their metrics")
        pb <- txtProgressBar(min = 0, max = nrow(cloud), style = 3)
        final <- cloud[, {setTxtProgressBar(pb, .GRP) ; knn_metrics(.SD, cloud_b, k, radius, basic, distribution, dimensionality, n_replicates)}, by = seq_len(nrow(cloud))]
        final <- final[, 2:ncol(final)]
      }
    }
  } else if(parallel == TRUE) { ###If parallel is TRUE-----------------------------------------------------------------------

    if(is.null(cores) == TRUE) {
      stop("Select the number of cores")
    }

    cl <- makeCluster(cores, outfile="")
    registerDoSNOW(cl)

    if(class(cloud)[1] == "neighborhood") {  ####For a object of neighborhood

      if(names(cloud$parameter) == "radius") {
        radius <- cloud$parameter
      } else {
        radius <- NULL
      }

      print("Calculating metrics")
      pb <- txtProgressBar(min = 0, max = nrow(cloud$cloud), style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress=progress)

      final <- foreach(i = 1:nrow(cloud$cloud), .inorder = FALSE, .combine= rbind, .packages = c("data.table", "rTLS"), .options.snow = opts) %dopar% {

        final <- data.table(points = i)

        if(basic == TRUE) {
          basc <- cloud$neighborhood[points == i, basic_metrics(.SD, radius = radius), by = points]
          final <- cbind(final, basc[,2:ncol(basc)])
        }

        if(distribution == TRUE) {
          disp <- cloud$neighborhood[points == i, distribution(.SD, radius = radius, n_replicates = n_replicates), by = points]
          final <- cbind(final, disp[,2:ncol(disp)])
        }

        if(dimensionality == TRUE) {
          dimen <- cloud$neighborhood[points == i, dimensionality(.SD), by = points]
          final <- cbind(final, dimen[,2:ncol(dimen)])
        }

        final
      }

      close(pb)

      final <- final[order(points)] ###Order of the results

      final <- cbind(cloud$cloud, final[, 2:ncol(final)]) ###Cbind with the point cloud

    } else if(class(cloud)[1] != "neighborhood") { ###For a object of class data.table

      if(method == "sphere") {  ###If method is sphere
        print("calculating spheres and their metrics")
        pb <- txtProgressBar(min = 0, max = nrow(cloud), style = 3)
        progress <- function(n) setTxtProgressBar(pb, n)
        opts <- list(progress=progress)

        final <- foreach(i = 1:nrow(cloud), .inorder = FALSE, .combine= rbind, .packages = c("data.table", "rTLS"), .options.snow = opts) %dopar% {
          final <- sphere_metrics(cloud[i,], cloud_b, radius, basic, distribution, dimensionality, n_replicates)
          final
        }

        close(pb)

      } else if(method == "knn") { ###If method is knn
        print("calculating knn and their metrics")
        pb <- txtProgressBar(min = 0, max = nrow(cloud), style = 3)
        progress <- function(n) setTxtProgressBar(pb, n)
        opts <- list(progress=progress)

        final <- foreach(i = 1:nrow(cloud), .inorder = FALSE, .combine= rbind, .packages = c("data.table", "rTLS"), .options.snow = opts) %dopar% {
          final <- knn_metrics(cloud[i,], cloud_b, k, radius, basic, distribution, dimensionality, n_replicates)
          final
        }

        close(pb)

      }
    }
    stopCluster(cl)
  }
  return(final)
}
