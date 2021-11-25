#' @title Voxels Summary
#'
#' @description Create a summary objects of class \code{"voxels"} created using the \code{\link{voxels}}.
#'
#' @param voxels An object of class \code{voxels} created using the \code{voxels()} function or a \code{data.table} describing the voxels coordinates and their number of points produced using \code{voxels()}.
#' @param edge_length A positive \code{numeric} vector with the voxel-edge length for the x, y, and z coordinates. This need to be used if \code{class(voxels) != "voxels"}. It use the same dimensional scale of the point cloud.
#' @param bootstrap Logical, if \code{TRUE} it computes a bootstrap on the H index calculations. \code{FALSE} as default.
#' @param R A positive \code{integer} of length 1 indicating the number of bootstrap replicates. This need to be used if \code{bootstrap = TRUE}.
#'
#' @return A \code{data.table} with with the summary of \code{voxels}.
#'
#' @details The function provides 12 main statistics of the voxels. Specifically, the first three columns represent the edge length of the voxels, the following three columns (ei. \code{N_voxels}, \code{Volume}, \code{Surface}) describe the number of voxels created, the total volume that they represent, and the surface area that they cover.
#' Following columns represent the mean (\code{Density_mean}) and sd (\code{Density_sd}) of the density of points per voxel (e.g. points/m2). Columns 9:12 provide metrics calculated using the Shannon Index. Specifically, \code{H} describe the entropy, \code{H_max} the maximum entropy, \code{Equitavility} the ratio between \code{H} and \code{Hmax}, and \code{Negentropy} describe the product of \code{Hmax} - \code{H}.
#' If \code{bootstrap = TRUE} four more columns are created (13:16). These represent the \code{mean} and \code{sd} of the H index estimated using bootstrap (\code{H_boot_mean} and \code{H_boot_sd}), the \code{Equtavility_boot} as the ratio of the ratio between \code{H_boot_sd} and \code{Hmax}, and \code{Negentropy_boot} as the product \code{Hmax} - \code{H_boot_mean}.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @importFrom boot boot
#' @importFrom stats sd
#'
#' @seealso \code{\link{voxels}}, \code{\link{voxels_counting}}, \code{\link{plot_voxels}}
#'
#' @examples
#' data("pc_tree")
#'
#' #Apply a summary on a object of class "voxels" using bootstrap with 1000 replicates.
#' vox <- voxels(pc_tree, edge_length = c(0.5, 0.5, 0.5))
#' summary_voxels(vox, bootstrap = TRUE, R = 1000)
#'
#' #Apply a summary on a product from 'voxels' using bootstrap with 1000 replicates.
#' vox <- voxels(pc_tree, edge_length = c(0.5, 0.5, 0.5), obj.voxels = FALSE)
#' summary_voxels(vox, edge_length = c(0.5, 0.5, 0.5), bootstrap = TRUE, R = 1000)
#'
#' @export
summary_voxels <- function(voxels, edge_length = NULL, bootstrap = FALSE, R = NULL) {

  if(class(voxels)[1] != "voxels") {
    if(is.null(edge_length) == TRUE) {
      stop("edge_length need to be defined")
    }

    colnames(voxels) <- c("X", "Y", "Z", "N")
    Edge.length <- edge_length

  } else if(class(voxels)[1] == "voxels") {
    Edge.length <- voxels$parameter
    voxels <- voxels$voxels
  }

  volumen <- Edge.length[1]*Edge.length[2]*Edge.length[3]

  N_voxels <- nrow(voxels)
  Volume <- (volumen)*N_voxels
  Surface <- nrow(unique(voxels[, c("X", "Y")]))*(Edge.length[1]*Edge.length[2])
  Density_mean <- mean(voxels$N/(Edge.length[1]*Edge.length[2]))
  Density_sd <- sd(voxels$N/(Edge.length[1]*Edge.length[2]))
  H <- shannon(voxels$N) #H index
  Hmax <- shannon(rep(1, nrow(voxels))) #H max
  Equitavility <- H/Hmax #Equitavility
  Negentropy <- Hmax - H #Negentropy

  if(bootstrap == FALSE ) {
      frame <- data.table(Edge.X = Edge.length[1], Edge.Y = Edge.length[2], Edge.Z = Edge.length[3], N_voxels, Volume, Surface, Density_mean, Density_sd, H, Hmax, Equitavility, Negentropy)

  } else if(bootstrap == TRUE) {

    if(is.null(R) == TRUE) {
      stop("Select the number of bootstrap replicates (R)")
    }

    h_boot <- boot(voxels$N, shannon_boot, R= R)$t
    H_boot_mean <- mean(h_boot) #H index with boot
    H_boot_sd <- sd(h_boot)
    Equitavility_boot <- H_boot_mean/Hmax #Equitavility based on boot
    Negentropy_boot <- Hmax - H_boot_mean #Negentropy based on boot

    frame <- data.table(Edge.X = Edge.length[1], Edge.Y = Edge.length[2], Edge.Z = Edge.length[3], N_voxels, Volume, Surface, Density_mean, Density_sd, H, Hmax, Equitavility, Negentropy, H_boot_mean, H_boot_sd, Equitavility_boot, Negentropy_boot)
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
