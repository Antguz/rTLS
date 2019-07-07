#' @title Dispersion, Agregation, and Mean Distance of Neighboring Points.
#'
#' @description Estimate the dispersion, aggregation, and mean distance of the neighboring points to a target point..
#'
#' @param space A \code{data.table} with *XYZ* coordinates of the neighboring points in the first three columns and a four column with their distance to the target point.
#' @param radius A \code{numeric} vector of a length 1 representing the radius of the sphere which was considered. If \code{NULL}, it use the maximun distance of \code{space} (\code{max(space[,4])}). \code{NULL} as default.
#' @param n_replicates An \code{interger} of a length 1 representing the number of replicates to estimate the expected distance. If \code{NULL}, it use the same number of rows of \code{space}. \code{NULL} as default.
#'
#' @return A \code{data.table} with four metrics: i) observed mean distance, ii) expected mean distance, iii) points dispersion, iv) mean aggregation to the target point.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @details The observed mean distance is estimated based on the average value of \code{space[,4]}.
#' The expected mean distance is computed on the average of \link{runif} values replicated n times using a range between 0 and \code{radius}.
#' The points dispersion is the ratio of observed mean distance and expected mean distance. The mean aggregation is estimated based on \code{mean(1 - space[4]/radius}.
#'
#' @seealso \code{\link{basic_metrics}}, \code{\link{dimensionality}}, \code{\link{cloud_metrics}}, \code{\link{neighborhood}}
#'
#' @examples
#' data("pc_tree")
#' neig <- neighborhood(pc_tree[50,], pc_tree, method = "sphere", radius = 0.2)
#' distribution(neig$neighborhood[, c(2:5)], radius = neig$parameter)
#'
#' @export
distribution <- function(space, radius = NULL, n_replicates = NULL) {

  colnames(space) <- c("X", "Y", "Z", "distance")
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
