#' @title Minimun distance between points
#'
#' @description Estimate the minimum distance between points in a point cloud.
#'
#' @param cloud A \code{data.table} with xyz coordinates in the first three columns.
#' @param parallel Logical, if \code{TRUE} it use a parallel processing to estimate distances between points. \code{FALSE} as default.
#' @param cores An \code{integer} >= 0 describing the number of cores use. This need to be used if \code{parallel = TRUE}.
#'
#' @return A \code{numeric} vector describing the minimun distance between points.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#' @examples
#' data("pc_tree")
#'
#' ###Estimate the minimun distance of a point cloud
#' min_distance(pc_tree)
#'
#' ###Estimate the minimun distance of a point cloud using parallel processing with 4 cores
#' min_distance(pc_tree, parallel = TRUE, cores = 4)
#'
#' @export
min_distance <- function(cloud, parallel = FALSE, cores = NULL) {

  if(parallel == FALSE) {

    print("Calculating distance between points")
    pb <- txtProgressBar(min = 0, max = nrow(cloud), style = 3) #Set progress bar

    results <- cloud[, {setTxtProgressBar(pb, .GRP) ; point_distance(.SD, cloud)}, by = seq_len(nrow(cloud))]

    minimum <- min(results$V1)

  } else if(parallel == TRUE) {

    if(is.null(cores) == TRUE) {
      stop("Select the number of cores")
    }

    cl <- makeCluster(cores, outfile="")
    registerDoSNOW(cl)

    print("Calculating distance between points using parallel processing")
    pb <- txtProgressBar(min = 0, max = nrow(cloud), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress=progress)

    results <- foreach(i = 1:nrow(cloud), .inorder = FALSE, .combine= rbind, .packages = c("data.table", "rTLS"), .options.snow = opts) %dopar% {
      results <- data.table(mininum = point_distance(x = cloud[i,], cloud = cloud))
      return(results)
    }

    minimum <- min(results$mininum)

    close(pb)
    stopCluster(cl)

  }

  return(minimum)
}

point_distance <- function(x, cloud) {

  xcoor <- as.numeric(x[1,1]) ###Set coordinates of interest
  ycoor <- as.numeric(x[1,2])
  zcoor <- as.numeric(x[1,3])

  space <- sqrt((xcoor - cloud[,1])^2 + (ycoor - cloud[,2])^2 + (zcoor - cloud[,3])^2) #Get the distance of the points
  space <- space[space$X > 0, ]
  space <- min(space)
  return(space)
}