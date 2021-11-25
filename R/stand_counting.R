#' @title Stand Counting
#'
#' @description Applies the \code{\link{voxels_counting}} function on a grid base point cloud.
#'
#' @param cloud A \code{data.table} of a point cloud with xyz coordinates in the first three columns.
#' @param xy.res A positive \code{numeric} vector describing the grid resolution of the xy coordinates to perform.
#' @param z.res A positive \code{numeric} vector of length 1 describing the vertical resolution. If \code{z.res = NULL} vertical profiles are not used.
#' @param points.min A positive \code{numeric} vector of length 1 minimum number of points to retain a sub-grid.
#' @param min_size A positive \code{numeric} vector of length 1 describing the minimum cube edge length to perform. This is required if \code{edge_sizes = NULL}.
#' @param edge_sizes A positive \code{numeric} vector describing the edge length of the different cubes to perform within each subgrid when \code{z.res = NULL}. If \code{edge_sizes = NULL}, it uses the maximum range of values for the xyz coordinates.
#' @param length_out A positive \code{interger} of length 1 indicating the number of different edge lengths to use for each subgrid. This is required if \code{edge_sizes  = NULL}.
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
#'
#' @seealso \code{\link{voxels_counting}}, \code{\link{voxels}}, \code{\link{summary_voxels}}
#'
#' @return A \code{data.table} with the summary of the voxels per grid created with their features.
#' @author J. Antonio Guzmán Q.
#' @examples
#'
#' data(pc_tree)
#'
#' #Applying stand_counting.
#' \donttest{
#' stand_counting(pc_tree, xy.res = c(4, 4), min_size = 3)
#' }
#'
#' #Applying stand_counting using bootstrap in the H index.
#' \donttest{
#' stand_counting(pc_tree,
#'                xy.res = c(4, 4),
#'                min_size = 3,
#'                bootstrap = TRUE,
#'                R = 10)
#' }
#'
#'
#' @export
stand_counting <- function(cloud, xy.res, z.res = NULL, points.min = NULL, min_size, edge_sizes = NULL, length_out = 10, bootstrap = FALSE, R = NULL, progress = TRUE, parallel = FALSE, threads = NULL) {

  if(is.null(z.res)) { ###Without vertical grid
    vox <- voxels(cloud, edge_length = c(xy.res[1], xy.res[2], xy.res[1]*10))
    vox$voxels <- vox$voxels[ , sum(N), by = .(X, Y)]
    colnames(vox$voxels)[3] <- "N"

    if(is.null(points.min)) { ###Min number of points
      vox$voxels <- vox$voxels[,1:2]
    } else {
      vox$voxels <- vox$voxels[N >= points.min]
      vox$voxels <- vox$voxels[,1:2]
    }
  } else { #With vertical grid
    vox <- voxels(cloud, edge_length = c(xy.res[1], xy.res[2], z.res))

    if(is.null(points.min)) { ###Min number of points
      vox$voxels <- vox$voxels[,1:3]
    } else {
      vox$voxels <- vox$voxels[N >= points.min]
      vox$voxels <- vox$voxels[,1:3]
    }
  }

  #Cube size if z.res is true
  if(is.null(z.res) != TRUE) {
    edge_sizes <- seq(from = log10(max(c(xy.res, z.res))), to = log10(min_size), length.out = length_out)
    edge_sizes <- 10^edge_sizes
  }

  if(parallel == TRUE) { ###If parallel is true

    cl <- makeCluster(threads, outfile="") #Make clusters
    registerDoSNOW(cl)

    if(progress == TRUE) {
      cat("Estimating stand_counting in parallel")  #Progress bar
      pb <- txtProgressBar(min = 1, max = nrow(vox$voxels), style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress=progress)

    } else {
      opts <- NULL
    }

    #Run in parallel
    results <- foreach(i = 1:nrow(vox$voxels), .inorder = FALSE, .combine= rbind, .packages = c("data.table", "rTLS"), .options.snow = opts) %dopar% {

      if(is.null(z.res)) {
        pixel <- cloud[X >= as.numeric((vox$voxels[i, 1] - (xy.res[1]/2))) &
                    X < as.numeric((vox$voxels[i, 1] + (xy.res[1]/2)))  &
                    Y >= as.numeric((vox$voxels[i, 2] - (xy.res[2]/2))) &
                    Y < as.numeric((vox$voxels[i, 2] + (xy.res[2]/2))),]

        frame <- voxels_counting(pixel,
                                 min_size = min_size,
                                 length_out = length_out,
                                 bootstrap = bootstrap,
                                 R = R,
                                 progress = FALSE,
                                 parallel = FALSE)

        final <- data.table(X = as.numeric(rep(vox$voxels[i,1], nrow(frame))),
                            Y = as.numeric(rep(vox$voxels[i,2], nrow(frame))))

        final <- cbind(final, frame)

      } else {
        pixel <- cloud[X >= as.numeric((vox$voxels[i, 1] - (xy.res[1]/2))) &
                    X < as.numeric((vox$voxels[i, 1] + (xy.res[1]/2)))  &
                    Y >= as.numeric((vox$voxels[i, 2] - (xy.res[2]/2))) &
                    Y < as.numeric((vox$voxels[i, 2] + (xy.res[2]/2)))  &
                    Z >= as.numeric((vox$voxels[i, 3] - (z.res/2))) &
                    Z < as.numeric((vox$voxels[i, 3] + (z.res/2))),]

        frame <- voxels_counting(pixel,
                                 edge_sizes = edge_sizes,
                                 min_size = min_size,
                                 length_out = length_out,
                                 bootstrap = bootstrap,
                                 R = R,
                                 progress = FALSE,
                                 parallel = FALSE)

        final <- data.table(X = as.numeric(rep(vox$voxels[i,1], nrow(frame))),
                            Y = as.numeric(rep(vox$voxels[i,2], nrow(frame))),
                            Z = as.numeric(rep(vox$voxels[i,3], nrow(frame))))

        final <- cbind(final, frame)
      }

      return(final)
    }

    #Stop clusters
    stopCluster(cl)

  } else if(parallel == FALSE) { ###If parallel is false

    if(progress == TRUE) {
      cat("Estimating stand_counting")
      pb <- txtProgressBar(min = 1, max = nrow(vox$voxels), style = 3) #Progress bar
    }

    #Run without using parallel
    results <- foreach(i = 1:nrow(vox$voxels), .inorder = FALSE, .combine= rbind) %do% {

      if(progress == TRUE) {
        setTxtProgressBar(pb, i)
      }

      if(is.null(z.res)) {
        pixel <- cloud[X >= as.numeric((vox$voxels[i, 1] - (xy.res[1]/2))) &
                      X < as.numeric((vox$voxels[i, 1] + (xy.res[1]/2)))  &
                      Y >= as.numeric((vox$voxels[i, 2] - (xy.res[2]/2))) &
                      Y < as.numeric((vox$voxels[i, 2] + (xy.res[2]/2))),]

        frame <- voxels_counting(pixel,
                                 min_size = min_size,
                                 length_out = length_out,
                                 bootstrap = bootstrap,
                                 R = R,
                                 progress = FALSE,
                                 parallel = FALSE)

        final <- data.table(X = as.numeric(rep(vox$voxels[i,1], nrow(frame))),
                            Y = as.numeric(rep(vox$voxels[i,2], nrow(frame))))

        final <- cbind(final, frame)

      } else {
        pixel <- cloud[X >= as.numeric((vox$voxels[i, 1] - (xy.res[1]/2))) &
                      X < as.numeric((vox$voxels[i, 1] + (xy.res[1]/2)))  &
                      Y >= as.numeric((vox$voxels[i, 2] - (xy.res[2]/2))) &
                      Y < as.numeric((vox$voxels[i, 2] + (xy.res[2]/2)))  &
                      Z >= as.numeric((vox$voxels[i, 3] - (z.res/2))) &
                      Z < as.numeric((vox$voxels[i, 3] + (z.res/2))),]

        frame <- voxels_counting(pixel,
                                 edge_sizes = edge_sizes,
                                 min_size = min_size,
                                 length_out = length_out,
                                 bootstrap = bootstrap,
                                 R = R,
                                 progress = FALSE,
                                 parallel = FALSE)

        final <- data.table(X = as.numeric(rep(vox$voxels[i,1], nrow(frame))),
                            Y = as.numeric(rep(vox$voxels[i,2], nrow(frame))),
                            Z = as.numeric(rep(vox$voxels[i,3], nrow(frame))))

        final <- cbind(final, frame)
      }

      return(final)
    }

    if(progress == TRUE) {
      close(pb) #Close progress
    }
  }

  #Vertical grid order
  if(is.null(z.res)) {
    results <- results[order(X, Y, N_voxels)]
  } else {
    results <- results[order(X, Y, Z, N_voxels)]
  }

  return(results)
}



