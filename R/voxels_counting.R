#' @title Voxels counting
#'
#' @description Creates voxels of different size on a point cloud using the \code{voxels()} function, and then return a summary of their features.
#'
#' @param cloud A \code{data.table} with xyz coordinates of the point clouds in the first three columns.
#' @param voxel.range A positive \code{numeric} vector describing the different voxel size to perform. If \code{NULL}, it use 10 voxel sizes by defaul based on the largest range of XYZ. See details.
#' @param bootstrap Logical, if \code{bootstrap = TRUE}, it computes a bootstrap on the H index calculations. \code{bootstrap = FALSE} as default.
#' @param R A positive \code{integer} of length 1 indicating the number of bootstrap replicates. This need to be used if \code{bootstrap = TRUE}.
#' @param random Logical. If \code{TRUE}, it generates voxels on a random point cloud created using the same number of points and _xyz_ range of \code{cloud}, and a minimun distance between points. \code{FALSE} as default.
#' @param minDistance A positive \code{numeric} vector of length 1 describing the future minimun distance between points. This need to be used if \code{random = TRUE}. If \code{random = TRUE} and \code{minDistance = NULL}, it automatically estimate the minimun distance using \code{min_distance()}.
#' @param parallel Logical, if \code{TRUE} it uses a parallel processing for the voxelization. If \code{random = TRUE} and \code{minDistance = NULL}, it also use parallel processing to estimate the minimun distance between points. \code{FALSE} as default.
#' @param cores An \code{integer} >= 0 describing the number of cores to use. This need to be used if \code{parallel = TRUE}.
#'
#' @return A \code{data.table} with the summary of the voxels created with their features.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#' @examples
#'
#' data(pc_tree)
#'
#' #Applying voxels counting
#' voxels_counting(pc_tree)
#'
#' #Voxels counting using boostrap on the H indixes with 1000 repetitions
#' voxels_counting(pc_tree, bootstrap = TRUE, R = 1000)
#'
#' #Voxels counting using bootstrap on the H indices with 1000 repetitions and applying parallel processing with 4 cores.
#' voxels_counting(pc_tree, bootstrap = TRUE, R = 1000, parallel = TRUE, cores = 4)
#'
#' @export
voxels_counting <- function(cloud, voxel.range = NULL, bootstrap = FALSE, R = NULL, random = FALSE, minDistance = NULL, parallel = FALSE, cores = NULL) {

  colnames(cloud) <- c("X", "Y", "Z")

  if(is.null(voxel.range) == TRUE) { ###Default voxel.range
    ranges <- c(max(cloud[,1]) - min(cloud[,1]), max(cloud[,2]) - min(cloud[,2]), max(cloud[,3]) - min(cloud[,3]))
    max.range <- ranges[which.max(ranges)] + 0.001
    voxel.range <- c(max.range, max.range/2, max.range/4, max.range/8, max.range/16, max.range/32, max.range/64, max.range/128, max.range/256, max.range/512, max.range/1024)
  }

  if(random == FALSE) {  ###If the voxelization will be performed on a non-random point cloud
    cloud_touse <- cloud

  } else if(random == TRUE) { ###If the  voxelization will be performed on a random point cloud

    if(is.null(minDistance) == TRUE) { ####If minimun distance is not defined
      point_min_distance <- min_distance(cloud, parallel = parallel, cores = cores)
    } else {
      point_min_distance <- minDistance ####If minimun distance is defined
    }

    range <- c(min(cloud$X), max(cloud$X), min(cloud$Y), max(cloud$Y), min(cloud$Z), max(cloud$Z)) ###Range of the point cloud

    cloud_touse <- random_cloud(nrow(cloud), range, point_min_distance) ###Creation of the random point cloud

  }

  if(parallel == TRUE) { ###If parallel is true

    cl <- makeCluster(cores, outfile="") #Make clusters
    registerDoSNOW(cl)

    print("Creating voxels in parallel")  #Progress bar
    pb <- txtProgressBar(min = 0, max = length(voxel.range), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress=progress)

    #Run in parallel
    results <- foreach(i = 1:length(voxel.range), .inorder = FALSE, .combine= rbind, .packages = c("data.table", "rTLS"), .options.snow = opts) %dopar% {
      vox <- voxels(cloud_touse, voxel.size = voxel.range[i], random = FALSE, obj.voxels = FALSE)
      summary <- summary_voxels(vox, voxel.size = voxel.range[i], bootstrap = bootstrap, R = R)
      return(summary)
    }

    results <- results[order(Voxel.size)]

    close(pb) #Close clusters
    stopCluster(cl)

  } else if(parallel == FALSE) { ###If parallel is false

    pb <- txtProgressBar(min = 0, max = length(voxel.range), style = 3) #Progress bar

    #Run without using parallel
    results <- foreach(i = 1:length(voxel.range), .inorder = FALSE, .combine= rbind) %do% {
      setTxtProgressBar(pb, i)
      vox <- voxels(cloud_touse, voxel.size = voxel.range[i], random = FALSE, obj.voxels = FALSE)
      summary <- summary_voxels(vox, voxel.size = voxel.range[i], bootstrap = bootstrap, R = R)
      return(summary)
    }
    results <- results[order(Voxel.size)]
  }
  return(results)
}

decimals <- function(x) {
  n <- 0
  while (!isTRUE(all.equal(floor(x),x)) & n <= 1e6) { x <- x*10; n <- n+1 }
  return (n)
}
