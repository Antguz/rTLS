#' @title Voxels summary
#'
#' @description Create a summary of the voxels or future voxels.
#'
#' @param voxels An object of class \code{voxels} created using the \code{voxels()} function.
#' @param bootstrap A logical vector length 1 vector. If \code{bootstrap = TRUE}, it compute a bootstrap on the H index calculations. \code{bootstrap = FALSE} as default.
#' @param R A positive \code{integer} of length 1 indicating the number of bootstrap replicates.
#'
#' @return A \code{data.table} with with the summary of \code{voxels}.
#'
#' @details The function estimate 9 main statistics of the voxels. Specifically, the first three columns (ei. \code{Voxel.size}, \code{N_voxels}, \code{Volumen}) describe the size of the voxel used, the number of voxels created, and the total volumen they represent.
#' Following columns represent the mean (\code{Density_mean}) and sd (\code{Density_sd}) of the density of points per voxel. Columns 6:10 provide metrics calculated using the Shannon index. Specifically, \code{H} describe the entropy, \code{H_max} the maximun entropy, \code{Equitavility} the ratio between \code{H} and \code{Hmax}, and \code{Negentropy} describe the product of \code{Hmax} - \code{H}.
#' If \code{bootstrap = TRUE} four more columns are created (10:13). These represent the mean and sd of the H index estimated using bootstrap (\code{H_boot_mean} and \code{H_boot_sd}), the \code{Equtavility_boot} as the ratio of the ratio between \code{H_boot_sd} and \code{Hmax}, and \code{Negentropy_boot} as the product \code{Hmax} - \code{H_boot_mean}.

#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @examples
#' data("pc_tree")
#'
#' ###Apply a summary on a product of voxels using bootstrap with 1000 replicates
#' vox <- voxels(pc_tree, voxel.size = 0.5)
#' summary_voxels(vox, bootstrap = TRUE, R = 1000)
#'
#'@export
summary_voxels <- function(voxels, bootstrap = FALSE, R = NULL) {

  if(class(voxels)[1] != "voxels") {
    stop("An object from voxels() need to be used")
  }

  Voxel.size <- voxels$parameter
  N_voxels <- nrow(voxels$voxels)
  Volumen <- (Voxel.size^3)*N_voxels
  Density_mean <- mean(voxels$voxels$N/(Voxel.size^3))
  Density_sd <- sd(voxels$voxels$N/(Voxel.size^3))
  H <- shannon(voxels$voxels$N) #H index
  Hmax <- shannon(rep(1, nrow(voxels$voxels))) #H max
  Equitavility <- H/Hmax #Equitavility
  Negentropy <- Hmax - H #Negentropy

  if(bootstrap == FALSE ) {
    frame <- data.table(Voxel.size, N_voxels, Volumen, Density_mean, Density_sd, H, Hmax, Equitavility, Negentropy)

  } else if(bootstrap == TRUE) {

    if(is.null(R) == TRUE) {
      stop("Select the number of bootstrap replicates (R)")
    }

    h_boot <- boot(voxels$voxels$N, shannon_boot, R= R)$t
    H_boot_mean <- mean(h_boot) #H index with boot
    H_boot_sd <- sd(h_boot)
    Equitavility_boot <- H_boot_mean/Hmax #Equitavility based on boot
    Negentropy_boot <- Hmax - H_boot_mean #Negentropy based on boot

    frame <- data.table(Voxel.size, N_voxels, Volumen, Density_mean, Density_sd, H, Hmax, Equitavility, Negentropy, H_boot_mean, H_boot_sd, Equitavility_boot, Negentropy_boot)
  }
  return(frame)
}

shannon <- function(n_points) {
  p.i <- n_points/sum(n_points)
  H <- (-1) * sum(p.i * log(p.i))
  return(H)
}

shannon_boot <- function(n_points, i) {
  n_boot <- n_points[i]
  p.i <- n_boot/sum(n_boot)
  H <- (-1) * sum(p.i * log(p.i))
  return(H)
}
