#' @title Point Cloud Sphere Covering
#'
#' @description Create spheres between k neighboring points in a cloud.
#'
#' @param cloud A \code{data.table} with *XYZ* coordinates in the first three columns.
#' @param k An \code{integer} vector equal or greater than 1 representing the number of k nearest neighbors to consider.
#' @param radius_max A \code{numeric} vector representing the maximum radius of the spheres to consider. If \code{NULL} all radius are considered.
#' @param build A \code{character} describing the search structure to be used: "kdtree", "kmeans", "linear".
#' @param threads An \code{integer} specifying the number of threads to use for parallel processing. Experiment to see what works best for your data on your hardware.
#' @param threads An \code{integer} specifying the number of threads to use for parallel processing. Experiment to see what works best for your data on your hardware.
#' @param checks Number of checks during searching. Higher value gives better search precision but takes longer. See FLANN C++ manual for more details.
#'
#' @details The function compute a sphere between to a point and its nearest
#' neighboring points, similar to Wang et al. 2020.
#'
#' @return A \code{data.table} describing the cartecian position of the spheres
#' in the cloud and their radius.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @references
#' Wang, D., Schraik, D., Hovi, A., & Rautiainen, M. (2020). Direct estimation
#' of photon recollision probability using terrestrial laser scanning.
#' Remote Sensing of Environment, 247, 111932.
#'
#'
#' @examples
#'
#' #Load data
#' data("pc_tree")
#'
#' #Run
#' sphere_covering(pc_tree, k = 4)
#'
#' @export
sphere_covering <- function(cloud, k = 4, radius_max = NULL, build = "kdtree", threads = 1L, checks = 1L) {

  #Estimate spheres using C++
  results <- sphere_covering_rcpp(as.matrix(cloud), k = k, build = build, threads = threads, checks = checks)

  #Create data table with names
  results <- as.data.table(results) #transform to data.table and provide names
  colnames(results) <- c("X", "Y", "Z", "radius", "k")

  #Unique spheres
  results <- unique(results, by = c("X", "Y", "Z", "radius")) ###Unique spheres

  #Subset by radius
  if(is.null(radius_max) != TRUE) {
    results <- results[radius <= radius_max, ]
  }

  results <- results[, c(1:4)]

  return(results)

}
