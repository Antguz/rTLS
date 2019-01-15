#' @import dplyr
#'
#' @title Basic metrics of the neighboring points.
#'
#' @description Estimate basic metrics of the surrunding neigboring points created based on a target point.
#'
#' @param space A \code{matrix} or \code{data.frame} with xzy coordinates in the first three columns of the neighboring points and a four columns with the distance to the target point.
#' @param radius A \code{numeric} vector of a length 1 representing the radius of the sphere that was consider. If \code{radius = NULL}, it use the maximun distance of \code{space} (\code{max(space[,4])}). \code{radius = NULL} as default.
#'
#' @return A \code{data.frame} with four metrics: i) number of neighboring points, ii) volume occupied by neighbors, iii) volume of the sphere used, and iv) density of points based on the volumen of the sphere. If \code{radius = NULL}, the volumnen occupied by neighbors and the volumen of the sphere are the same.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @examples
#' data("pc_tree")
#'
#' neig <- neighborhood(pc_tree[50,], pc_tree, method = "sphere", radius = 0.2)
#'
#' basic.metrics(neig$neighborhood$`1`, radius = neig$parameter)
#'
#' @export
basic.metrics <- function(space, radius = NULL) {

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
}
