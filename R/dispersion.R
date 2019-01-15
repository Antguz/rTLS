#' @import dplyr
#'
#' @title Dispersion, agregation, and mean distance neighboring points.
#'
#' @description Estimate the dispersion, aggregation, and mean distance of the neighboring points.
#'
#' @param space A \code{matrix} or \code{data.frame} with xzy coordinates in the first three columns of the neighboring points and a four columns with the distance to the target point.
#'
#' @return A  \code{matrix} with the xyz coordinates of the neighboring points and a fourth column with their distance.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @examples
#' data("pc_tree")
#'
#' neig <- neighborhood(pc_tree[50,], pc_tree, method = "sphere", radius = 0.2)
#'
#' dispersion(neig$neighborhood$`1`, radius = neig$parameter)
#'
#' @export
dispersion <- function(space, radius = NULL, n_replicates = NULL) {

  if(length(space[,1]) >= 3) {
    n_replicates <- ifelse(is.null(n_replicates) == TRUE, length(space[,4]), n_replicates)
    radius <- ifelse(is.null(radius) == TRUE, max(space[,4]), radius)

    frame <- data.frame(obs_distance = mean(space[,4]),
                        exp_distance = mean(replicate(n_replicates, mean(runif(length(space[,4]), min = 0, max = radius)))),
                        dispersion = obs_distance/exp_distance,
                        aggregation = mean(1 - space[,4]/radius))

  } else if(length(space[,1]) < 3) {

    frame <- data.frame(obs_distance = NA,
                        exp_distance = NA,
                        dispersion = NA,
                        aggregation = NA)
  }
  frame
}



