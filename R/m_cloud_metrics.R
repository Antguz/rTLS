#' @title Point cloud metrics at different scales
#'
#' @description Estimate different metrics on the poits of a cloud using different scales. It estimate 10 parameters based on Wang et al. 2017.
#'
#' @param cloud An object of class \code{"neighborhood"} or a \code{data.table} with xyz coordinates in the first three columns.
#' @param cloud_b A \code{data.table} with xyz coordinates in the first three columns. If \code{cloud_b} is \code{NULL}, \code{cloud_b == cloud}. \code{NULL} as default.
#' @param basic Logical, if \code{TRUE} it estimate basic metrics. \code{basic = TRUE} as default.
#' @param distribution Logical, if \code{TRUE} it estimate distribution metrics of points. \code{distribution = TRUE} as default.
#' @param dimensionality Logical, if \code{TRUE} it estimate dimensionality metrics. \code{dimensionality = TRUE} as default.
#' @param method A character string specifying the method to estimate neighbors. It most be one of \code{"sphere"} or \code{"knn"}.
#' @param radius.range A positive \code{numeric} vector representing the range of radius of the spheres to consider. This need be used if \code{method = "sphere"}. This may be used if \code{method = "knn"}, but this need to be a numeric vector of length 1.
#' @param k.range An integer of a length 1 representing the number of neighbors to consider. This need be used if \code{method = "knn"}.
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
#'
#' data("pc_tree")
#'
#' ###On objects of class neighborhood
#' ##Calculate the neighborhood of 1000 random rows of a point cloud using the sphere method and a radius of 0.2.
#' cloud.random <- pc_tree[sample(nrow(pc_tree), 1000), ]
#' dist <- neighborhood(cloud.random, pc_tree, method = "sphere", radius = 1)
#'
#' #Estimate on three different scales without parallel
#' m.cloud_metrics(dist, radius.range = c(1, 0.9, 0.8), parallel = TRUE, cores = 4)
#'
#' #Estimate metrics three different scales (1, 0.9, 0.8)
#' m.cloud_metrics(dist, radius.range = c(1, 0.9, 0.8), parallel = TRUE, cores = 4)
#'
#'
#' @export
m_cloud_metrics <- function(cloud, cloud_b = NULL, basic = TRUE, distribution = TRUE, dimensionality = TRUE, method = NULL, radius.range = NULL, k.range, n_replicates = NULL, parallel = FALSE, cores) {

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
        final <- merge(final, metrics, by = c("X", "Y", "Z"), all.x = TRUE, all.y = TRUE)
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
        final <- merge(final, metrics, by = c("X", "Y", "Z"), all.x = TRUE, all.y = TRUE)
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
        final <- merge(final, metrics, by = c("X", "Y", "Z"), all.x = TRUE, all.y = TRUE)
      }

    } else if(method == "knn") { ### If knn is selected
      if(length(radius.range) > 1) {
        stop("The radius.range for the knn method need to be a positive numeric vector")
      }

      cloud_neig <- neighborhood(cloud = cloud, cloud_b = cloud_b, method = "knn", radius = radius.range, k = max(k.range), parallel = parallel, cores = cores)

      final <- cloud_neig$cloud[,1:3]

      for(i in 1:length(k.range)) {
        print(paste("", i, " of ", length(k.range), " scales", sep = ""))
        new_cloud <- sub_neighborhood(cloud, method = "knn", new_k = k.range[i])
        metrics <- cloud_metrics(new_cloud,  basic = basic, distribution = distribution, dimensionality = dimensionality, n_replicates = n_replicates, parallel = parallel, cores = cores)
        colnames(metrics) <- c("X", "Y", "Z", paste("", colnames(metrics[,4:ncol(metrics)]), "_(", k.range[i], ")", sep = ""))
        final <- merge(final, metrics, by = c("X", "Y", "Z"), all.x = TRUE, all.y = TRUE)
      }
    }
  }
  return(final)
}
