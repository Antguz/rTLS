#' @title Voxels counting
#'
#' @description On a point cloud create voxels of different size using the \code{voxels} function, and then return a summary of their features.
#'
#' @param cloud A \code{data.table} with xyz coordinates in the first three columns.
#' @param voxel.range A positive numeric vector describing the different voxel size to perform. If \code{voxel.range = NULL}, it use 10 voxel sizes by defaul. See details.
#' @param bootstrap Logical, if \code{bootstrap = TRUE}, it compute a bootstrap on the H index calculations. \code{bootstrap = FALSE} as default.
#' @param R A positive \code{integer} of length 1 indicating the number of bootstrap replicates.
#' @param parallel Logical, if \code{TRUE} it use a parallel processing. \code{FALSE} as default.
#' @param cores An \code{integer} >= 0 describing the number of cores use. This need to be used if \code{parallel = TRUE}.
#'
#' @return A \code{data.table} with the summary with their features.
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
voxels_counting <- function(cloud, voxel.range = NULL, bootstrap = FALSE, R, parallel = FALSE, cores) {

  if(is.null(voxel.range) == TRUE) { ###Default voxel.range
    ranges <- c(max(cloud[,1]) - min(cloud[,1]), max(cloud[,2]) - min(cloud[,2]), max(cloud[,3]) - min(cloud[,3]))
    max.range <- ranges[which.max(ranges)]*2
    voxel.range <- c(max.range, max.range/2, max.range/4, max.range/8, max.range/16, max.range/32, max.range/64, max.range/128, max.range/256, max.range/512)
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
      vox <- voxels(cloud, voxel.size = voxel.range[i])
      summary <- summary_voxels(vox, bootstrap = bootstrap, R = R)
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
      vox <- voxels(cloud, voxel.size = voxel.range[i])
      summary <- summary_voxels(vox, bootstrap = bootstrap, R = R)
      return(summary)
    }

    results <- results[order(Voxel.size)]

  }
  return(results)
}
