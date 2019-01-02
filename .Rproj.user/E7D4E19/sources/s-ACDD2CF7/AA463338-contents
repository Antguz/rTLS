#' @import dplyr
#'
#' @title Subsample neighboring points
#'
#' @description Subsample the neighboring points based on a distance or knn
#'
#' @param x An object of class \code{neighborhood}.
#' @param method A character string specifying the method to subsample the neighboring points. It most be one of \code{"distance"} or \code{"knn"}.
#' @param new_distance A \code{numeric} vector of a length 1 representing the new distance to consider. This will be used if \code{method = "distance"}.
#' @param new_knn An integer of a length 1 representing the number of neighbors to consider. This will be used if \code{method = "knn"}.
#' @param
#'
#' @return An object of class "neighborhood" with a list per point of the neighboring points.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @examples
#' data("pc_tree")
#'
#' ###Subset neighboring points from 0.2 to 0.1
#' dis <- neighborhood(pc_tree, method = "distance", radius = 0.2, parallel = FALSE)
#' sub_neighborhood(dis, new_distance = 0.1)
#'
#' @export
sub_neighborhood <- function(x, method, new_distance, new_knn) {

  if(method == "distance") {
  sub <- llply(dis,
    .fun = function(x, new_distance) {x <- x[x[, 4] <= new_distance,]},
     new_distance = new_distance,
    .inform = FALSE,
    .parallel = FALSE)

  } else if(method == "knn") {


  }

  return(sub)
}
