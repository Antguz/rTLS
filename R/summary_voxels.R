#' @import dplyr
#'
#' @title Voxels summary
#'
#' @description Create a summary of the voxels created using the function \code{voxels}.
#'
#' @param data A \code{data.frame} with xyz coordinates of the voxels in the first three columns and a fourth column with the number of points in each voxel.
#' @param voxel.size A positive \code{numeric} vector indicating the size used in \code{voxels}. It is needed to estimated metrics of density.1
#' @param bootstrap A logical vector length 1 vector. If \code{bootstrap = TRUE}, it compute a bootstrap on the H index calculations. \code{bootstrap = FALSE} as default.
#' @param R A positive \code{integer} of length 1 indicating the number of bootstrap replicates.
#' @return A \code{data.frame} with with the summary of \code{data}.
#' @details The function estimate 9 main statistics of the voxels. Specifically, the first three columns (ei. \code{Voxel.size}, \code{N_voxels}, \code{Volumen}) describe the size of the voxel used, the number of voxels created, and the total volumen they represent.
#' Following columns represent the mean (\code{Density_mean}) and sd (\code{Density_sd}) of the density of points per voxel. Columns 6:10 provide metrics calculated using the Shannon index. Specifically, \code{H} describe the entropy, \code{H_max} the maximun entropy, \code{Equitavility} the ratio between \code{H} and \code{Hmax}, and \code{Negentropy} describe the product of \code{Hmax} - \code{H}.
#' If \code{bootstrap = TRUE} four more columns are created (10:13). These represent the mean and sd of the H index estimated using bootstrap (\code{H_boot_mean} and \code{H_boot_sd}), the \code{Equtavility_boot} as the ratio of the ratio between \code{H_boot_sd} and \code{Hmax}, and \code{Negentropy_boot} as the product \code{Hmax} - \code{H_boot_mean}.
#'
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @examples
#' data("pc_tree")
#'
#' ###Create voxels of a size of 0.5.
#' vox <- voxels(pc_tree, voxel.size = 0.5)
#'
#' ###Summary voxels
#' summary_voxels(vox)
#'
#' ###Summary voxels and estimated the entropy using bootstrap with 1000 replicates
#' summary_voxels(vox, bootstrap = TRUE, R = 1000)
#'
#'@export
summary_voxels <- function(vox.obj, bootstrap = FALSE, R) {

  if(class(vox.obj) != "voxels") {
    stop("vox.obj need to be an object of class voxels. Create it using the voxels function")
  }

  voxel.size <- vox.obj$parameter
  n_voxels <- length(vox.obj$voxels[,1]) #Number de voxels
  volumen <- (voxel.size^3)*n_voxels
  density_mean <- mean(vox.obj$voxels[,4]/(voxel.size^3)) #Mean density of points
  density_sd <- sd(vox.obj$voxels[,4]/(voxel.size^3)) #Sd of the density of points
  H <- shannon(vox.obj$voxels[,4]) #H index
  vox.obj$voxels$eq <- 1
  Hmax <- shannon(vox.obj$voxels$eq) #H max
  equitavility <- H/Hmax #Equitavility
  negentropy <- Hmax - H #Negentropy

  if(bootstrap == FALSE ) {
    frame <- data.frame(voxel.size, n_voxels, volumen, density_mean, density_sd, H, Hmax, equitavility, negentropy)

  } else if(bootstrap == TRUE) {
    h_boot <- boot(vox.obj$voxels[,4], shannon_boot, R= R)$t
    H_boot_mean <- mean(h_boot) #H index with boot
    H_boot_sd <- sd(h_boot)
    equitavility_boot <- H_boot_mean/Hmax #Equitavility based on boot
    negentropy_boot <- Hmax - H_boot_mean #Negentropy based on boot

    frame <- data.frame(voxel.size, n_voxels, volumen, density_mean, density_sd, H, Hmax, equitavility, negentropy, H_boot_mean, H_boot_sd, equitavility_boot, negentropy_boot)
  }
  row.names(frame) <- c()
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
