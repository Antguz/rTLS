#' @title Dispersion, agregation, and mean distance of neighboring points.
#'
#' @description Estimate the dispersion, aggregation, and mean distance of the neighboring points to a target point..
#'
#' @param space A \code{data.table} with xzy coordinates in the first three columns of the neighboring points and a four columns with the distance to the target point.
#' @param radius A \code{numeric} vector of a length 1 representing the radius of the sphere which was considered. If \code{radius = NULL}, it use the maximun distance of \code{space} (\code{max(space[,4])}). \code{radius = NULL} as default.
#' @param n_replicates An \code{interger} of a length 1 representing the number of replicates to estimate the expected distance. If \code{n_replicates = NULL}, it use the same number of rows of \code{space}. \code{n_replicates = NULL} as default.
#'
#' @return A \code{data.frame} with four metrics: i) observed distance, ii) expected distance, iii) points dispersion, iv) aggregation to the target point.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @examples
#' data("pc_tree")
#'
#' neig <- neighborhood(pc_tree[50,], pc_tree, method = "sphere", radius = 0.2)
#'
#' distribution(neig$neighborhood[, c(2:5)], radius = neig$parameter)
#'
#' @export
distribution <- function(space, radius = NULL, n_replicates = NULL) {

  space <- na.exclude(space)

  if(nrow(space) >= 3) {
    n_replicates <- ifelse(is.null(n_replicates) == TRUE, nrow(space), n_replicates)
    radius <- ifelse(is.null(radius) == TRUE, max(space$distance), radius)

    obs_distance <- mean(space$distance)
    exp_distance <- mean(replicate(n_replicates, mean(runif(nrow(space), min = 0, max = radius))))
    dispersion <- obs_distance/exp_distance
    aggregation <- mean(1 - space$distance/radius)

    frame <- data.table(obs_distance,
                        exp_distance,
                        dispersion,
                        aggregation)

  } else if(nrow(space) < 3) {

    frame <- data.table(obs_distance = as.numeric(NA),
                        exp_distance = as.numeric(NA),
                        dispersion = as.numeric(NA),
                        aggregation = as.numeric(NA))
  }
  return(frame)
}
