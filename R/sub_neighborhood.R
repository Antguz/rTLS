#' @title Subsample Neighboring Points
#'
#' @description Subsample the neighboring points based in a radius of a shpere or knn.
#'
#' @param neig.obj An object of class \code{"neighborhood"}.
#' @param method A \code{character} string specifying the method to subsample the neighboring points. It most be one of \code{"sphere"} or \code{"knn"}.
#' @param new_radius A \code{numeric} vector of a length 1 representing the new radius to consider. This will be used if \code{method = "sphere"}. To be functional, \code{"new_radius"} most be lower than \code{"radius"} used in \code{\link{neighborhood}}.
#' @param new_k An integer of a length 1 representing the new number of neighbors to consider. This will be used if \code{method = "knn"}. To be functional, \code{"new_k"} most be lower than \code{"k"} used in \code{\link{neighborhood}}.
#'
#' @return An object of class \code{\link{neighborhood}} with a list per point of the neighboring points.
#' @author J. Antonio Guzmán  Q.
#'
#' @importFrom data.table .I
#'
#' @seealso \code{\link{knn_neighbors}}, \code{\link{sphere_neighbors}}, \code{\link{neighborhood}}
#'
#' @examples
#' data("pc_tree")
#'
#' #' ##Calculate the neighborhood of 1000 random rows of a point cloud using the sphere method and a radius of 0.2.
#' cloud.random <- pc_tree[sample(nrow(pc_tree), 1000), ]
#' dist <- neighborhood(cloud.random, pc_tree, method = "sphere", radius = 0.2)
#'
#' ###Subset neighboring points from a radius 0.2 to a radius of 0.1
#' sub_neighborhood(dist, method = "sphere", new_radius = 0.1)
#'
#' @export
sub_neighborhood <- function(neig.obj, method, new_radius, new_k) {

  if(method == "sphere") {
    sub <- neig.obj$neighborhood[distance <= new_radius]
    parameter <- new_radius
    names(parameter) <- "radius"

  } else if(method == "knn") {
    sub <- neig.obj$neighborhood[neig.obj$neighborhood[, .I[1:new_k], points]$V1]
    parameter <- new_k
    names(parameter) <- "k"
  }

  if(length(unique(neig.obj$neighborhood$points)) != length(unique(sub$points))) { ###If there are points without values
    frame <- data.table(points = unique(neig.obj$neighborhood$points))
    sub <- merge(frame, sub, by = "points", all.x = TRUE)
    final <- list(cloud = neig.obj$cloud, parameter = parameter, neighborhood = sub)

  } else {
    final <- list(cloud = neig.obj$cloud, parameter = parameter, neighborhood = sub)
  }

  class(final) <- "neighborhood"
  return(final)
}
