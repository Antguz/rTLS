#' @title Basic metrics of the neighboring points.
#'
#' @description It estimate basic metrics of the surrounding neighboring points created based on a target point.
#'
#' @param space A \code{data.table} with *XYZ* coordinates of the neighboring points in the first three columns and a four column with their distance to the target point.
#' @param radius A \code{numeric} vector of a length 1 representing the radius of the sphere that was used. If \code{radius = NULL}, it uses the maximum distance of \code{space} (\code{max(space[,4])}). \code{radius = NULL} as default.
#'
#' @return A \code{data.table} with four metrics: i) the number of neighboring points, ii) the volume occupied by neighbors, iii) volume of the sphere used, and iv) density of points based on the sphere size. If \code{radius = NULL}, the volume occupied by neighbors and the volume of the sphere is the same.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @examples
#' data("pc_tree")
#'
#' neig <- neighborhood(pc_tree[50,], pc_tree, method = "sphere", radius = 0.2)
#'
#' basic_metrics(neig$neighborhood[, 2:5], radius = neig$parameter)
#'
#' @export
basic_metrics <- function(space, radius = NULL) {

  space <- na.exclude(space)

  if(nrow(space) >= 1) {
    radius <- ifelse(is.null(radius) == TRUE, max(space$distance), radius)
    n_neig <- nrow(space)
    neig_volumen <- ((4/3)*pi*(max(space$distance)^3))
    sphere_volumen <- ((4/3)*pi*(radius^3))
    density <- n_neig/sphere_volumen

    frame <- data.table(n_neig, neig_volumen, sphere_volumen, density)

  } else if(nrow(space) < 1) {
    frame <- data.table(n_neig = as.integer(NA),
                        neig_volumen = as.numeric(NA),
                        sphere_volumen = as.numeric(NA),
                        density = as.numeric(NA))

  }

  return(frame)
}
