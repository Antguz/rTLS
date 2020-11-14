#' @title Point Cloud Sphere Covering
#'
#' @description Create spheres between two neighboring points in a cloud.
#'
#' @param cloud A \code{data.table} with *XYZ* coordinates in the first three columns.
#' @param radius A \code{numeric} vector representing the maximun radius of the spheres to consider.
#' @param kmax An \code{integer} vector equal or greather than 1 representing the number of k nearest neighbors to consider if a points present more than k nearst neighbors with the same distance beteween them.
#' @param threads An \code{integer} specifying the number of threads to use for parallel processing. Experiment to see what works best for your data on your hardware.
#' @param progress Show progress. \code{TRUE} as default.
#'
#' @details The function compute a sphere between to a point and its nearst neighboring point, similar to Wang et al. 2020.
#' If a point present two or more nearst neighboring points with the same distance, \code{kmax} could be used to retrieve
#' a maximun of k-nearst points (e.g. usefull in resample point clouds where several points may share the same distance to a target point).
#'
#' @return A \code{data.table} describing the cartesian position of the spheres in the cloud and their radius.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @references
#' Wang, D., Schraik, D., Hovi, A., & Rautiainen, M. (2020). Direct estimation of photon recollision probability using terrestrial laser scanning. Remote Sensing of Environment, 247, 111932.
#'
#'
#' @examples
#' ###Estimate the dimensionality on a sample of 100 points.
#' #Load data
#' data("pc_tree")
#'
#' #Run
#' sphere_covering(pc_tree, 0.2)
#'
#' @export
sphere_covering <- function(cloud, radius, kmax = 1, threads = 1, progress = TRUE) {

  results <- sphere_covering_rcpp(as.matrix(cloud), radius, kmax, threads, progress) #Sphere covering from rcpp

  results <- as.data.table(results) #transform to data.table and provide names
  colnames(results) <- c("X", "Y", "Z", "radius")

  results <- unique(results) #Filter unique spheres

  return(results)

}
