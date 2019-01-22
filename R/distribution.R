#' @title Dispersion, agregation, and mean distance neighboring points.
#'
#' @description Estimate the dispersion, aggregation, and mean distance of the neighboring points to a target point..
#'
#' @param space A \code{matrix} or \code{data.frame} with xzy coordinates in the first three columns of the neighboring points and a four columns with the distance to the target point.
#' @param radius A \code{numeric} vector of a length 1 representing the radius of the sphere which was considered. If \code{radius = NULL}, it use the maximun distance of \code{space} (\code{max(space[,4])}). \code{radius = NULL} as default.
#' @n_replicates An \code{interger} of a length 1 representing the number of replicates to estimate the expected distance. If \code{n_replicates = NULL}, it use the same number of rows of \code{space}. \code{n_replicates = NULL} as default.
#'
#' @return A \code{data.frame} with four metrics: i) observed distance, ii) expected distance, iii) points dispersion, iv) aggregation to the target point.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @examples
#' data("pc_tree")
#'
#' neig <- neighborhood(pc_tree[50,], pc_tree, method = "sphere", radius = 0.2)
#'
#' distribution(neig$neighborhood$`1`, radius = neig$parameter)
#'
#' @export
distribution <- function(space, radius = NULL, n_replicates = NULL) {

  if(length(space[,1]) >= 3) {
    n_replicates <- ifelse(is.null(n_replicates) == TRUE, length(space[,4]), n_replicates)
    radius <- ifelse(is.null(radius) == TRUE, max(space[,4]), radius)

    obs_distance <- mean(space[,4])
    exp_distance <- mean(replicate(n_replicates, mean(runif(length(space[,4]), min = 0, max = radius))))
    dispersion <- obs_distance/exp_distance
    aggregation <- mean(1 - space[,4]/radius)

    frame <- data.frame(obs_distance,
                        exp_distance,
                        dispersion,
                        aggregation)

  } else if(length(space[,1]) < 3) {

    frame <- data.frame(obs_distance = NA,
                        exp_distance = NA,
                        dispersion = NA,
                        aggregation = NA)
  }
  return(frame)
}



