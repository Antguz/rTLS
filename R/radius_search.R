#' Radius Search of Points
#'
#' Radius searching of points based on FLANN C++
#'
#' @param query A \code{data.table} containing the set of query points where each row represent a point and each column a given coordinate.
#' @param ref A \code{numeric} containing the set of reference points where each row represent a point and each column a given coordinate.
#' @param radius A \code{numeric} describing the euclidean distance form the each query points.
#' @param max_neighbour An \code{integer} specifying the maximun number of points to look around each query point for a given radius.
#' @param same Logic. If \code{TRUE}, it delete neighbors with distance of 0, useful when the k search is based on the same query.
#' @param build A \code{character} describing the search structure to be used: "kdtree", "kmeans", "linear".
#' @param threads An \code{integer} specifying the number of threads to use for parallel processing. Experiment to see what works best for your data on your hardware. If 0, then the maximum allowable cores are used.
#' @param checks Number of checks during searching. Higher value gives better search precision but takes longer. See FLANN C++ manual for more details.
#'
#' @return A \code{data.table} with three columns describing the indices of the query and ref points and the distances.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @details
#' This function is based on the FLAAN C++ Library (Muja and Lowe 2009) for a fast
#' estimation of neighbors points. It is adapted from rflann (Muja, Lowe, Yee, 2018) to
#' simplify the workflow with TLS data. If you use this function, please consider cite
#' both documents.
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
#' #Radius search of 0.1
#' radius_search(pc_tree, pc_tree, radius = 0.1, 100, TRUE, "kdtree")
#'
#' @export
radius_search <- function(query, ref, radius, max_neighbour, same = FALSE, build = "kdtree", threads = 1L, checks = 1L) {

  results <- radius_search_rcpp(as.matrix(query), as.matrix(ref), radius, max_neighbour, same, build, threads, checks)
  results <- as.data.table(results)
  colnames(results) <- c("query", "ref", "distance")

  return(results)

}

