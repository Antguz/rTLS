#' @title Point cloud metrics
#'
#' @description Estimate different metrics on the poits of a cloud. It estimate 10 parameters based on Wang et al. 2017.
#'
#' @param cloud A \code{matrix} or \code{data.frame} with xyz coordinates in the first three columns or an object of class \code{neighborhood}.
#' @param cloud_b A \code{matrix} or \code{data.frame} with xyz coordinates in the first three columns. If \code{cloud_b} is \code{NULL}, \code{cloud_b == cloud}. \code{NULL} as default.
#' @param basic Logical, if \code{TRUE} it estimate basic metrics. \code{basic = TRUE} as default.
#' @param distribution Logical, if \code{TRUE} it estimate distribution metrics of points. \code{distribution = TRUE} as default.
#' @param dimensionality Logical, if \code{TRUE} it estimate dimensionality metrics. \code{dimensionality = TRUE} as default.
#' @param method A character string specifying the method to estimated the neighbors. It most be one of \code{"sphere"} or \code{"knn"}.
#' @param radius A \code{numeric} vector of a length 1 representing the radius of the sphere to consider. This need be used if \code{method = "sphere"} and this may used if \code{method = "knn"}.
#' @param k An integer of a length 1 representing the number of neighbors to consider. This need be used if \code{method = "knn"}.
#' @param parallel Logical, if \code{TRUE} it use a parallel processing.
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
#' ###Run from an object of class data.frame or matrix
#' cloud_metrics(pc_tree, method = "sphere", radius = 0.2)
#'
#' ###Run from an object of class neighborhood
#' dist <- neighborhood(pc_tree, method = "sphere", radius = 0.2, parallel = FALSE)
#' cloud_metrics(dist, parallel = FALSE)
#'
#'@export
cloud_metrics <- function(cloud, cloud_b = NULL, basic = TRUE, distribution = TRUE, dimensionality = TRUE, method, radius, k, n_replicates = NULL, parallel = FALSE) {

  if(class(cloud) != "neighborhood") {  ####For matrix or data.frame
    print("Calculating neighbors")
    cloud <- neighborhood(cloud, cloud_b, method = method, radius = radius, k, parallel = parallel)
  }

  final <- cloud$cloud
  radius <- ifelse(names(cloud$parameter) == "radius", cloud$parameter, NULL)

  if(basic == TRUE) {
    print("Calculating basic metrics")
    basc <- ldply(cloud$neighborhood, .fun = function(space, radius = NULL) {

      if(length(space[,1]) >= 1) {
        radius <- ifelse(is.null(radius) == TRUE, max(space[,4]), radius)

        n_neig <- length(space[,4])
        neig_volumen <- ((4/3)*pi*(max(space[,4])^3))
        sphere_volumen <- ((4/3)*pi*(radius^3))
        density <- n_neig/sphere_volumen

        frame <- data.frame(n_neig, neig_volumen, sphere_volumen, density)

      } else if(length(space[,1]) < 1) {
        frame <- data.frame(n_neig = NA, neig_volumen = NA, sphere_volumen = NA, density = NA)

      }
      return(frame)
    }, radius = radius, .progress = "text", .parallel = parallel, .id = NULL)
    final <- cbind(final, basc)
  }

  if(distribution == TRUE) {
    print("Calculating distribution metrics")
    disp <- ldply(cloud$neighborhood, .fun = function(space, radius = NULL, n_replicates = NULL) {

      if(length(space[,1]) >= 3) {
        n_replicates <- ifelse(is.null(n_replicates) == TRUE, length(space[,4]), n_replicates)
        radius <- ifelse(is.null(radius) == TRUE, max(space[,4]), radius)

        obs_distance <- mean(space[,4])
        exp_distance <- mean(replicate(n_replicates, mean(runif(length(space[,4]), min = 0, max = radius))))
        dispersion <- obs_distance/exp_distance
        aggregation <- mean(1 - space[,4]/radius)

        frame <- data.frame(obs_distance,
                            exp_distance,
                            dispersion,
                            aggregation)

      } else if(length(space[,1]) < 3) {

        frame <- data.frame(obs_distance = NA,
                            exp_distance = NA,
                            dispersion = NA,
                            aggregation = NA)
      }
      frame
    }, radius = radius, n_replicates = n_replicates, .progress = "text", .parallel = parallel, .id = NULL)
    final <- cbind(final, disp)
  }

  if(dimensionality == TRUE) {
    print("Calculating dimensionality metrics")
    dimen <- ldply(cloud$neighborhood, .fun = function(space) {
      if(length(space[,1]) >= 3) {
        pca <- prcomp(space[,1:3], center = TRUE, scale = FALSE, retx = FALSE)
        eigval <- pca$sdev^2

        frame <- data.frame(linearity = (eigval[1]-eigval[2])/eigval[1],
                            planarity = (eigval[2]-eigval[3])/eigval[1],
                            scattering = eigval[3]/eigval[1],
                            omnivariance = (eigval[1]*eigval[2]*eigval[3])^(1/3),
                            anisotropy = (eigval[1]-eigval[3])/eigval[1],
                            eigenentropy = -((eigval[1] * log(eigval[1])) + (eigval[2] * log(eigval[2])) + (eigval[3] * log(eigval[3]))),
                            sum_eigen = sum(eigval),
                            sur_var = min(eigval)/sum(eigval),
                            eigen_ratio = eigval[2]/eigval[2])


      } else if(length(space[,1]) < 3) {
        frame <- data.frame(linearity = NA,
                            planarity = NA,
                            scattering = NA,
                            omnivariance = NA,
                            anisotropy = NA,
                            eigenentropy = NA,
                            sum_eigen = NA,
                            sur_var = NA,
                            eigen_ratio = NA)
      }
      frame
    }, .progress = "text", .parallel = parallel, .id = NULL)
    final <- cbind(final, dimen)
  }
  return(final)
}
