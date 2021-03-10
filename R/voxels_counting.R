#' @title Voxels Counting
#'
#' @description Creates cube like voxels of different size on a point cloud using the \code{\link{voxels}} function, and then return a \code{\link{summary_voxels}} of their features.
#'
#' @param cloud A \code{data.table} with xyz coordinates of the point clouds in the first three columns.
#' @param edge_sizes A positive \code{numeric} vector describing the edge length of the different cubes to perform. If \code{NULL}, it use edge sizes by default based on the largest range of XYZ and \code{min_size}.
#' @param min_size A positive \code{numeric} vector of length 1 describing the minimum cube edge length to perform. This is required if \code{edge_sizes = NULL}.
#' @param length_out A positive \code{interger} of length 1 indicating the number of different edge lengths to use. This is required if \code{edge_sizes  = NULL}.
#' @param bootstrap Logical. If \code{TRUE}, it computes a bootstrap on the H index calculations. \code{FALSE} as default.
#' @param R A positive \code{integer} of length 1 indicating the number of bootstrap replicates. This need to be used if \code{bootstrap = TRUE}.
#' @param progress Logical, if \code{TRUE} displays a graphical progress bar. \code{TRUE} as default.
#' @param parallel Logical, if \code{TRUE} it uses a parallel processing for the voxelization. \code{FALSE} as default.
#' @param threads An \code{integer} >= 0 describing the number of threads to use. This need to be used if \code{parallel = TRUE}.
#'
#' @import data.table
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom foreach %do%
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#'
#' @seealso \code{\link{voxels}}, \code{\link{summary_voxels}}, \code{\link{plot_voxels}}
#'
#' @return A \code{data.table} with the summary of the voxels created with their features.
#' @author J. Antonio Guzmán Q.
#' @examples
#'
#' data(pc_tree)
#'
#' #Applying voxels counting.
#' voxels_counting(pc_tree, min_size = 2)
#'
#' #Voxels counting using bootstrap on the H indexes with 1000 repetitions.
#' voxels_counting(pc_tree, min_size = 2, bootstrap = TRUE, R = 1000)
#'
#'
#' @export
voxels_counting <- function(cloud, edge_sizes = NULL, min_size, length_out = 10, bootstrap = FALSE, R = NULL, progress = TRUE, parallel = FALSE, threads = NULL) {

  colnames(cloud) <- c("X", "Y", "Z")

  if(is.null(edge_sizes) == TRUE) { ###Default edge_sizes
    ranges <- c(max(cloud[,1]) - min(cloud[,1]), max(cloud[,2]) - min(cloud[,2]), max(cloud[,3]) - min(cloud[,3]))
    max.range <- ranges[which.max(ranges)] + 0.0001
    edge_sizes <- seq(from = log10(c(max.range)), to = log10(min_size), length.out = length_out)
    edge_sizes <- 10^edge_sizes
  }

  cloud_touse <- cloud

  if(parallel == TRUE) { ###If parallel is true

    cl <- makeCluster(threads, outfile="") #Make clusters
    registerDoSNOW(cl)

    if(progress == TRUE) {
      print("Creating voxels in parallel")  #Progress bar
      pb <- txtProgressBar(min = 0, max = length(edge_sizes), style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress=progress)

    } else {
      opts <- NULL
    }

    #Run in parallel
    results <- foreach(i = 1:length(edge_sizes), .inorder = FALSE, .combine= rbind, .packages = c("data.table", "rTLS"), .options.snow = opts) %dopar% {
      vox <- voxels(cloud_touse, edge_length = c(edge_sizes[i], edge_sizes[i], edge_sizes[i]), obj.voxels = FALSE)
      summary <- summary_voxels(vox, edge_length = c(edge_sizes[i], edge_sizes[i], edge_sizes[i]), bootstrap = bootstrap, R = R)
      return(summary)
    }

    results <- results[order(Edge.X)]

    stopCluster(cl)

  } else if(parallel == FALSE) { ###If parallel is false

    if(progress == TRUE) {
      print("Creating voxels")
      pb <- txtProgressBar(min = 0, max = length(edge_sizes), style = 3) #Progress bar
    }

    #Run without using parallel
    results <- foreach(i = 1:length(edge_sizes), .inorder = FALSE, .combine= rbind) %do% {

      if(progress == TRUE) {
        setTxtProgressBar(pb, i)
      }

      vox <- voxels(cloud_touse, edge_length = c(edge_sizes[i], edge_sizes[i], edge_sizes[i]), obj.voxels = FALSE)
      summary <- summary_voxels(vox, edge_length = c(edge_sizes[i], edge_sizes[i], edge_sizes[i]), bootstrap = bootstrap, R = R)
      return(summary)
    }

    if(progress == TRUE) {
      close(pb) #Close clusters
    }

    results <- results[order(Edge.X)]
  }
  return(results)
}

decimals <- function(x) {
  n <- 0
  while (!isTRUE(all.equal(floor(x),x)) & n <= 1e6) { x <- x*10; n <- n+1 }
  return (n)
}

