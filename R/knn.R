#' K Nearest Neighbors
#'
#' Adapted K nearest neighbors based on RcppHNSW
#'
#' @param query A \code{data.table} containing the set of query points where each row represent a point and each column a given coordinate.
#' @param ref A \code{numeric} containing the set of reference points where each row represent a point and each column a given coordinate.
#' @param k An \code{integer} describing the number of nearest neighbors to search for.
#' @param threads An \code{integer} specifying the number of threads to use for parallel processing. Experiment to see what works best for your data on your hardware. If 0, then the maximum allowable cores are used.
#' @param ... Arguments passed to \code{hnsw_build} and \code{hnsw_search}.
#' @return A \code{data.table} with three columns describing the indices of the query, ref, and k neighbors and the distances.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @details
#' This function is based on hnswlib C++ library (Malkov & Yashunin 2016) and
#' its bindings for R (RcppHNSW; Melville 2020) for a fast estimation of neighbors
#' points. It is adapted to simplify the workflow within rTLS.
#' If you use this function, please consider cite the C++ library and
#' \code{RcppHNSW} package.
#'
#' @references
#' Muja, M., Lowe, D.G. (2009). Fast approximate nearest neighbors with automatic algorithm configuration. VISAPP (1), 2(331-340), 2.
#'
#' Muja, M., Lowe, D.G., Yee, J., (2018). rflann: Basic R Interface to the 'FLANN' C++ Library. R package version 1.6.0.
#'
#' @seealso \code{\link{radius_search}}
#'
#' @examples
#'
#' #Point cloud
#' data("pc_tree")
#'
#' #knn search using k = 3
#' knn(pc_tree, pc_tree, k = 3, same = TRUE)
#'
#' @export
knn <- function(query, ref, k, distance = "euclidean", same = FALSE, threads = 1L, progress = FALSE, ...) {

  #Initinal arguments
  if(progress == TRUE) {
    bar <- "bar"
  } else if(progress == FALSE) {
    bar <- NULL
  }

  if(same == TRUE) {
    k_final = k+1
  } else if(same == FALSE) {
    k_final = k
  }

  #Modifications and estimation using RcppHNSW
  dist <- match.arg(distance, c("l2", "euclidean", "cosine", "ip"))

  neig <- hnsw_build(X = as.matrix(ref),
                     distance = dist,
                     progress = bar,
                     n_threads = threads,
                     ...)

  results <- hnsw_search(X = as.matrix(query),
                         ann = ann,
                         k = k_final,
                         progress = bar,
                         n_threads = threads,
                         ...)

  #Index
  index <- cbind(query = 1:nrow(query),
                 data.table(results$idx))
  colnames(index) <- c("query", as.character(c(1:k_final)))
  index_melt <- melt(index,
                     id.vars = c("query"),
                     variable.name = "k_index",
                     value.name = "ref")
  #Distance
  dist <- cbind(query = 1:nrow(query),
                data.table(results$dist))
  colnames(dist) <- c("query", as.character(c(1:k_final)))
  dist_melt <- melt(dist,
                    id.vars = c("query"),
                    variable.name = "k_index",
                    value.name = "distance")

  #Merge index and distance
  results <- merge(index_melt, dist_melt, by = c("query", "k_index"))
  results <- results[, c(1,3,2,4)]
  results$k_index <- as.numeric(as.character(results$k_index))

  if(same == TRUE) {
    results$k_index <- results$k_index - 1
    results <- subset(results, k_index != 0)
  }

  #Export
  return(results)

}

