#' @import dplyr
#'
#' @title Point cloud metrics
#'
#' @description Estimate different metrics on the poits of a cloud. It estimate 10 parameters based on Wang et al. 2017.
#'
#' @param cloud A \code{matrix} or \code{data.frame} with xyz coordinates in the first three columns or an object of class \code{neighborhood}.
#' @param cloud_b A \code{matrix} or \code{data.frame} with xyz coordinates in the first three columns. If \code{cloud_b} is \code{NULL}, \code{cloud_b == cloud}. \code{NULL} as default.
#' @param basic Logical, if \code{TRUE} it estimate basic metrics. \code{basic = TRUE} as default.
#' @param dispersion Logical, if \code{TRUE} it estimate dispersion metrics. \code{dispersion = TRUE} as default.
#' @param dimensionality Logical, if \code{TRUE} it estimate dimensionality metrics. \code{dimensionality = TRUE} as default.
#' @param method A character string specifying the method to estimated the neighbors. It most be one of \code{"sphere"} or \code{"knn"}.
#' @param radius A \code{numeric} vector of a length 1 representing the radius of the sphere to consider. This need be used if \code{method = "sphere"} and this may used if \code{method = "knn"}.
#' @param k An integer of a length 1 representing the number of neighbors to consider. This need be used if \code{method = "knn"}.
#' @param parallel Logical, if \code{TRUE} it use a parallel processing.
#' @return A \code{data.frame} with the estimated parameters.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @seealso basic.metrics, dispersion, dimensionality
#'
#' @references Wang, D., Hollaus, M., & Pfeifer, N. (2017). Feasibility of machine learning methods for separating wood and leaf points from Terrestrial Laser Scanning data. ISPRS Annals of Photogrammetry, Remote Sensing & Spatial Information Sciences, 4.
#'
#' @examples
#' data("pc_tree")
#'
#' ###Run from an object of class data.frame or matrix
#' cloud_metrics(pc_tree, method = "sphere", radius = 0.2)
#'
#' ###Run from an object of class neighborhood
#' dist <- neighborhood(pc_tree, method = "sphere", radius = 0.2, parallel = FALSE)
#' cloud_metrics(dist, method = "sphere", radius = 0.2, parallel = FALSE)
#'
#'@export
cloud_metrics <- function(cloud, cloud_b = NULL, basic = TRUE, dispersion = TRUE, dimensionality = TRUE, method, radius, k, n_replicates = NULL, parallel = FALSE) {

  if(class(cloud) != "neighborhood") {  ####For matrix or data.frame
    print("Calculating neighbors")
    cloud <- neighborhood(cloud, cloud_b, method = method, radius = radius, k, parallel = parallel)
  }

  final <- cloud$cloud
  radius <- ifelse(names(cloud$parameter) == "radius", cloud$parameter, NULL)

  if(basic == TRUE) {
    print("Calculating basic metrics")
    basc <- ldply(cloud$neighborhood, .fun = basic.metrics, radius = radius, .progress = "text", .parallel = parallel, .id = NULL)
    final <- cbind(final, basc)
  }

  if(dispersion == TRUE) {
    print("Calculating dispersion metrics")
    disp <- ldply(cloud$neighborhood, .fun = dispersion, radius = radius, n_replicates = n_replicates, .progress = "text", .parallel = parallel, .id = NULL)
    final <- cbind(final, disp)
  }

  if(dimensionality == TRUE) {
    print("Calculating dimensionality metrics")
    dimen <- ldply(cloud$neighborhood, .fun = dimensionality, .progress = "text", .parallel = parallel, .id = NULL)
    final <- cbind(final, dimen)
  }

  return(final)
}
