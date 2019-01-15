#' @import dplyr
#'
#' @title Spheres summary
#'
#' @description Create a summary of the spheres created using the function \code{cloud_metrics}.
#'
#' @param data A \code{data.frame} with xyz coordinates of the used points in the first three columns and nine more columns of metrics estimated from \code{cloud_dimensionality}.
#' @param radius A positive \code{numeric} vector indicating the radius of the sphere used. It is needed to estimated metrics of density.
#' @param bootstrap A logical vector length 1 vector. If \code{bootstrap = TRUE}, it compute a bootstrap on the H index calculations. \code{bootstrap = FALSE} as default.
#' @param R A positive \code{integer} of length 1 indicating the number of bootstrap replicates.
#' @return A \code{data.frame} with with the summary of \code{data}.
#'
#' @details The function estimate 9 main statistics of the spheres. Specifically, the first three columns (ei. \code{Sphere.radius}, \code{N_spheres}, \code{Volumen}) describe the size of the sphere used, the number of spheres created, and the total volumen they represent.
#' Following columns represent the mean (\code{Density_mean}) and sd (\code{Density_sd}) of the density of points per sphere. Columns 6:10 provide metrics calculated using the Shannon index based on the frequency of points per sphere. Specifically, \code{H} describe the entropy, \code{H_max} the maximun entropy, \code{Equitavility} the ratio between \code{H} and \code{Hmax}, and \code{Negentropy} describe the product of \code{Hmax} - \code{H}.
#' If \code{bootstrap = TRUE} four more columns are created (10:13). These represent the mean and sd of the H index estimated using bootstrap (\code{H_boot_mean} and \code{H_boot_sd}), the \code{Equtavility_boot} as the ratio of the ratio between \code{H_boot_sd} and \code{Hmax}, and \code{Negentropy_boot} as the product \code{Hmax} - \code{H_boot_mean}.
#'
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @examples
#' data("pc_tree")
#'
#' ###Estimate the dimensionality of a point cloud using a sphere of a radius of 0.2.
#' dimen <- cloud_dimensionality(pc_tree[1:100,], method = "sphere", radius = 0.2, parallel = FALSE)
#'
#' ###Summary voxels
#' summary_sphere(dimen, radius = 0.5)
#'
#' ###Summary voxels and estimated the entropy using bootstrap with 1000 replicates
#' summary_sphere(dimen, radius = 0.5, bootstrap = TRUE, R = 1000)
#'
#'@export
summary_sphere <- function(data, radius, bootstrap = FALSE, R) {

  Sphere.radius <- radius
  N_spheres <- length(data$n) #Number de voxels
  Volumen <- ((4/3)*pi*(radius^3))*N_spheres
  Density_mean <- mean(data$n/((4/3)*pi*(radius^3))) #Mean density of points
  Density_sd <- sd(data$n/((4/3)*pi*(radius^3))) #Sd of the density of points
  H <- shannon(data$n) #H index
  data$eq <- 1
  Hmax <- shannon(data$eq) #H max
  Equitavility <- H/Hmax #Equitavility
  Negentropy <- Hmax - H #Negentropy

  if(bootstrap == FALSE ) {
    frame <- data.frame(Sphere.radius, N_spheres, Volumen, Density_mean, Density_sd, H, Hmax, Equitavility, Negentropy)

  } else if(bootstrap == TRUE) {
    h_boot <- boot(data$n, shannon_boot, R= R)$t
    H_boot_mean <- mean(h_boot) #H index with boot
    H_boot_sd <- sd(h_boot)
    Equitavility_boot <- H_boot_mean/Hmax #Equitavility based on boot
    Negentropy_boot <- Hmax - H_boot_mean #Negentropy based on boot

    frame <- data.frame(Sphere.radius, N_spheres, Volumen, Density_mean, Density_sd, H, Hmax, Equitavility, Negentropy, H_boot_mean, H_boot_sd, Equitavility_boot, Negentropy_boot)
  }
  return(frame)
}
