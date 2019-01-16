#' @import dplyr
#'
#' @title Subsample neighboring points
#'
#' @description Subsample the neighboring points based in a radius of a shpere or knn.
#'
#' @param neig.obj An object of class \code{neighborhood}.
#' @param method A character string specifying the method to subsample the neighboring points. It most be one of \code{"sphere"} or \code{"knn"}.
#' @param new_radius A \code{numeric} vector of a length 1 representing the new radius to consider. This will be used if \code{method = "sphere"}. To be functional, \code{"new_radius"} most be lower than \code{"radius"} used in \code{"neighborhood"}.
#' @param new_k An integer of a length 1 representing the new number of neighbors to consider. This will be used if \code{method = "knn"}.  To be functional, \code{"new_k"} most be lower than \code{"k"} used in \code{"neighborhood"}.
#'
#' @return An object of class "neighborhood" with a list per point of the neighboring points.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @examples
#' data("pc_tree")
#'
#' ###Subset neighboring points from a radius 0.2 to a radius of 0.1
#' dis <- neighborhood(pc_tree, method = "sphere", radius = 0.2, parallel = FALSE)
#' sub_neighborhood(dis, method = "sphere", new_radius = 0.1)
#'
#' @export
sub_neighborhood <- function(neig.obj, method, new_radius, new_k) {

  if(method == "sphere") {
    sub <- llply(neig.obj$neighborhood,
                 .fun = function(x, new_radius) {x <- x[x[, 4] <= new_radius,]},
                 new_radius = new_radius,
                 .inform = FALSE,
                 .parallel = FALSE)

    parameter <- new_radius
    names(parameter) <- "radius"

    final <- list(cloud = neig.obj$cloud, parameter = parameter, neighborhood = sub)

  } else if(method == "knn") {
    sub <- llply(neig.obj$neighborhood,
                 .fun = function(x, new_k) {x <- x[1:new_k,]},
                 new_k = new_k,
                 .inform = FALSE,
                 .parallel = FALSE)

    parameter <- new_k
    names(parameter) <- "k"

    final <- list(cloud = neig.obj$cloud, parameter = parameter, neighborhood = sub)

  }
  class(final) <- "neighborhood"
  return(final)
}
