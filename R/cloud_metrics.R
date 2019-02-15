#' @title Point cloud metrics
#'
#' @description Estimate different metrics on the poits of a cloud. It estimate 10 parameters based on Wang et al. 2017.
#'
#' @param cloud A \code{data.table} with xyz coordinates in the first three columns or an object of class \code{"neighborhood"}.
#' @param cloud_b A \code{data.table} with xyz coordinates in the first three columns. If \code{cloud_b} is \code{NULL}, \code{cloud_b == cloud}. \code{NULL} as default.
#' @param basic Logical, if \code{TRUE} it estimate basic metrics. \code{basic = TRUE} as default.
#' @param distribution Logical, if \code{TRUE} it estimate distribution metrics of points. \code{distribution = TRUE} as default.
#' @param dimensionality Logical, if \code{TRUE} it estimate dimensionality metrics. \code{dimensionality = TRUE} as default.
#' @param method A character string specifying the method to estimated the neighbors. It most be one of \code{"sphere"} or \code{"knn"}.
#' @param radius A \code{numeric} vector of a length 1 representing the radius of the sphere to consider. This need be used if \code{method = "sphere"} and this may used if \code{method = "knn"}.
#' @param k An integer of a length 1 representing the number of neighbors to consider. This need be used if \code{method = "knn"}.
#' @param parallel Logical, if \code{TRUE} it use a parallel processing.
#' @param cores An \code{integer} >= 0 describing the number of cores use. This need to be used if  \code{parallel = TRUE}.
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
#' ###On objects of class neighborhood
#' ##Calculate the neighborhood of 1000 random rows of a point cloud using the sphere method and a radius of 0.2.
#' cloud.random <- pc_tree[sample(nrow(pc_tree), 1000), ]
#' dist <- neighborhood(cloud.random, pc_tree, method = "sphere", radius = 0.05)
#'
#' #Estimate metrics without using parallel.
#' cloud_metrics(dist)
#'
#' #Estimate matrics using parallel processing with 4 cores.
#' cloud_metrics(dist, parallel = TRUE, cores = 4)
#'
#' ###On objects of class data.table.
#' ##Select 1000 random rows of a point cloud.
#' cloud.random <- pc_tree[sample(nrow(pc_tree), 1000), ]
#'
#' #Estimate metrics without using parallel by creating spheres of a radius of 0.2.
#' cloud_metrics(cloud.random, pc_tree, method = "sphere", radius = 0.2)
#'
#' #Estimate metrics metrics in parallel using 4 cores by creating spheres of a radius of 0.2.
#' cloud_metrics(cloud.random, pc_tree, method = "sphere", radius = 0.2, parallel = TRUE, cores = 4)
#'
#'@export
cloud_metrics <- function(cloud, cloud_b = NULL, basic = TRUE, distribution = TRUE, dimensionality = TRUE, method = NULL, radius = NULL, k, n_replicates = NULL, parallel = FALSE, cores) {

  if(parallel == FALSE) { ###If parallel is false-----------------------------------------------------------------------

    if(class(cloud)[1] == "neighborhood") {  ####For a object of neighborhood
      final <- cloud$cloud
      radius <- ifelse(names(cloud$parameter) == "radius", cloud$parameter, NULL)

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

      radius <- ifelse(names(cloud$parameter) == "radius", cloud$parameter, NULL)

      print("Calculating metrics")
      pb <- txtProgressBar(min = 0, max = nrow(cloud$cloud), style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress=progress)

      final <- foreach(i = 1:nrow(cloud$cloud), .inorder = FALSE, .combine= rbind, .packages = c("data.table", "rTLS"), .options.snow = opts) %dopar% {

        final <- cloud$cloud[i]

        if(basic == TRUE) {
          basc <- basic_metrics(cloud$neighborhood[points == i, 2:5], radius = radius)
          final <- cbind(final, basc)
        }

        if(distribution == TRUE) {
          disp <- distribution(cloud$neighborhood[points == i, 2:5], radius = radius, n_replicates = n_replicates)
          final <- cbind(final, disp)
        }

        if(dimensionality == TRUE) {
          dimen <- dimensionality(cloud$neighborhood[points == i, 2:4])
          final <- cbind(final, dimen)
        }

        final
      }

      close(pb)

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
