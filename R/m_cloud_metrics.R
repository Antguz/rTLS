#' @title Point Cloud Metrics at Multiple Scales
#'
#' @description Estimate different metrics on the poits of a cloud using multiple scales.
#'
#' @param cloud An object of class \code{"neighborhood"} or \code{data.table} with *XYZ* coordinates in the first three columns.
#' @param cloud_b A \code{data.table} with *XYZ* coordinates in the first three columns. If \code{cloud_b = NULL}, \code{cloud_b == cloud}. \code{NULL} as default.
#' @param basic Logical, if \code{TRUE} it estimates \code{\link{basic_metrics}}. \code{basic = TRUE} as default.
#' @param distribution Logical, if \code{TRUE} it estimates \code{\link{distribution}} metrics of points. \code{distribution = TRUE} as default.
#' @param dimensionality Logical, if \code{TRUE} it estimates \code{\link{dimensionality}} metrics. \code{dimensionality = TRUE} as default.
#' @param method A \code{character} string specifying the method to estimate neighbors. It most be one of \code{"sphere"} or \code{"knn"}.
#' @param radius.range A positive \code{numeric} vector representing the range of spheres radius to consider. This need be used if \code{method = "sphere"}. This may be used if \code{method = "knn"}, but this should to be a \code{numeric} vector of length 1.
#' @param k.range An \code{integer} of a length 1 representing the number of neighbors to consider. This need be used if \code{method = "knn"}.
#' @param n_replicates An \code{interger} of a length 1 representing the number of replicates to estimate the expected distance. This needs to be used if \code{distribution = TRUE}.If \code{NULL}, it uses the number of neighbors per point. \code{NULL} as default.
#' @param parallel Logical, if \code{TRUE} it use a parallel processing.
#' @param cores An \code{integer} >= 0 describing the number of cores use. This need to be used if \code{parallel = TRUE}.
#'
#' @return A \code{data.table} with the estimated parameters.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @seealso \code{\link{basic_metrics}}, \code{\link{distribution}}, \code{\link{dimensionality}}, \code{\link{cloud_metrics}}, \code{\link{neighborhood}}
#'
#' @examples
#'
#' data("pc_tree")
#'
#' ###On objects of class neighborhood
#' ##Calculate the neighborhood of 1000 random rows of a point cloud using the sphere method and a radius of 0.2.
#' cloud.random <- pc_tree[sample(nrow(pc_tree), 100), ]
#' dist <- neighborhood(cloud.random, pc_tree, method = "sphere", radius = 5)
#'
#' #Estimate on three different scales scales of spheres (5, 2.5) without parallel
#' m_cloud_metrics(dist, radius.range = c(5, 2.5), n_replicates = 10, parallel = FALSE)
#'
#' \dontrun{
#' #Estimate metrics using three different scales of spheres (5, 2.5) with parallel
#' m_cloud_metrics(dist, radius.range = c(5, 2.5), n_replicates = 10, parallel = TRUE, cores = 4)
#' }
#'
#' @export
m_cloud_metrics <- function(cloud, cloud_b = NULL, basic = TRUE, distribution = TRUE, dimensionality = TRUE, method = NULL, radius.range = NULL, k.range, n_replicates = NULL, parallel = FALSE, cores = NULL) {

  if(class(cloud)[1] == "neighborhood") { ### For a object of neighborhood
    if(names(cloud$parameter) == "radius") { ###If method sphere is selected
      if(cloud$parameter < max(radius.range)) {
        stop("The maximun value of radius.range can not be greater than the radius parameter of cloud")
      }

      final <- cloud$cloud[,1:3]

      for(i in 1:length(radius.range)) {
        print(paste("", i, " of ", length(radius.range), " scales", sep = ""))
        new_cloud <- sub_neighborhood(cloud, method = "sphere", new_radius = radius.range[i])
        metrics <- cloud_metrics(new_cloud,  basic = basic, distribution = distribution, dimensionality = dimensionality, n_replicates = n_replicates, parallel = parallel, cores = cores)
        colnames(metrics) <- c("X", "Y", "Z", paste("", colnames(metrics[,4:ncol(metrics)]), "_(", radius.range[i], ")", sep = ""))
        final <- merge(final, metrics, by = c("X", "Y", "Z"))
      }
    }
    else if(names(cloud$parameter) == "k") { ###If method knn is selected
      if(cloud$parameter < max(k.range)) {
        stop("The maximun value of k.range can not be greater than the k parameter of cloud")
      }

      final <- cloud$cloud[,1:3]

      for(i in 1:length(k.range)) {
        print(paste("", i, " of ", length(k.range), " scales", sep = ""))
        new_cloud <- sub_neighborhood(cloud, method = "knn", new_k = k.range[i])
        metrics <- cloud_metrics(new_cloud,  basic = basic, distribution = distribution, dimensionality = dimensionality, n_replicates = n_replicates, parallel = parallel, cores = cores)
        colnames(metrics) <- c("X", "Y", "Z", paste("", colnames(metrics[,4:ncol(metrics)]), "_(", k.range[i], ")", sep = ""))
        final <- merge(final, metrics, by = c("X", "Y", "Z"))
      }
    }

  } else if(class(cloud)[1] != "neighborhood") { ### For a object of data.table
    if(is.null(method) == TRUE) {
      stop("A method need to be selected")
    }

    if(method == "sphere") { ### If sphere is selected
      cloud_neig <- neighborhood(cloud = cloud, cloud_b = cloud_b, method = "sphere", radius = max(radius.range), parallel = parallel, cores = cores)

      final <- cloud_neig$cloud[,1:3]

      for(i in 1:length(radius.range)) {
        print(paste("", i, " of ", length(radius.range), " scales", sep = ""))
        new_cloud <- sub_neighborhood(cloud, method = "sphere", new_radius = radius.range[i])
        metrics <- cloud_metrics(new_cloud,  basic = basic, distribution = distribution, dimensionality = dimensionality, n_replicates = n_replicates, parallel = parallel, cores = cores)
        colnames(metrics) <- c("X", "Y", "Z", paste("", colnames(metrics[,4:ncol(metrics)]), "_(", radius.range[i], ")", sep = ""))
        final <- merge(final, metrics, by = c("X", "Y", "Z"))
      }

    } else if(method == "knn") { ### If knn is selected
      if(length(radius.range) > 0) {
        stop("The radius.range for the knn method need to be a positive numeric vector")
      }

      cloud_neig <- neighborhood(cloud = cloud, cloud_b = cloud_b, method = "knn", radius = radius.range, k = max(k.range), parallel = parallel, cores = cores)

      final <- cloud_neig$cloud[,1:3]

      for(i in 1:length(k.range)) {
        print(paste("", i, " of ", length(k.range), " scales", sep = ""))
        new_cloud <- sub_neighborhood(cloud, method = "knn", new_k = k.range[i])
        metrics <- cloud_metrics(new_cloud,  basic = basic, distribution = distribution, dimensionality = dimensionality, n_replicates = n_replicates, parallel = parallel, cores = cores)
        colnames(metrics) <- c("X", "Y", "Z", paste("", colnames(metrics[,4:ncol(metrics)]), "_(", k.range[i], ")", sep = ""))
        final <- merge(final, metrics, by = c("X", "Y", "Z"))
      }
    }
  }
  return(final)
}
