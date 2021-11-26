#' Filtering of Point Clouds
#'
#' Filtering of point clouds using different methods
#'
#' @param cloud A \code{data.table} contain three columns representing the *XYZ* coordinates.
#' @param method A filtering method to use. It most be \code{"SOR"}, \code{"min_neighbors"}, or \code{"min_neighbors"}.
#' @param radius A \code{numeric} vector representing the radius of the sphere to consider. This needs to be used if \code{method = "voxel_center"}.
#' @param min_neighbours An \code{integer} representing the minimum number of neighbors to keep a given point. This needs to be used if \code{method = "min_n"}.
#' @param k An \code{integer} vector representing the number of neighbors to consider. This needs be used if \code{method = "SOR"}.
#' @param nSigma A \code{numeric} vector representing the standard deviation multiplier. This needs to be used if \code{method = "SOR"}.
#' @param edge_length A positive \code{numeric} vector with the voxel-edge length for the x, y, and z coordinates. This needs to be used if \code{method = "voxel_center"}.
#' @param distance Type of distance to calculate. \code{"euclidean"} as default. Look \code{hnsw_knn} for more options.
#' @param threads An \code{integer} specifying the number of threads to use for parallel processing. Experiment to see what works best for your data on your hardware.
#' @param verbose If TRUE, log messages to the console.
#' @param progress If TRUE, log a progress bar when \code{verbose = TRUE}. Tracking progress could cause a small overhead.
#' @param ... Arguments passed to \code{hnsw_build} and \code{hnsw_search}.
#'
#' @return A \code{data.table} with the filtered points
#' @author J. Antonio Guzmán Q.
#'
#' @examples
#'
#' \donttest{
#' #Load data
#' data("pc_tree")
#'
#' #Move pc_tree for comparison
#' pc_compare <- pc_tree
#' pc_compare$X <- pc_compare$X - 7
#'
#' #SOR filter
#' r1 <- filter(pc_tree, method = "SOR", k = 30, nSigma = 1)
#' rgl::plot3d(r1, col = "red") #Filter
#' rgl::points3d(pc_compare, col = "black") #Original
#'
#' #min_neighbours filter
#' r2 <- filter(pc_tree, "min_neighbors", radius = 0.02, min_neighbours = 20)
#' rgl::plot3d(r2, col = "red") #Filter
#' rgl::points3d(pc_compare, col = "black") #Original
#'
#' #voxel_center filter
#' r3 <- filter(pc_tree, method = "voxel_center", edge_length = 0.1)
#' rgl::plot3d(r3, col = "red") #Filter
#' rgl::points3d(pc_compare, col = "black") #Original
#' }
#'
#' @export
filter <- function(cloud, method, radius, min_neighbours, k, nSigma, edge_length, distance = "euclidean", threads = 1L, verbose = FALSE, progress = FALSE, ...) {

  if(method == "SOR") {

    point <- knn(cloud, cloud, (k+1), same = TRUE, distance = distance, verbose = verbose, progress = progress, ...)
    point <- point[, lapply(.SD, mean), by= query]
    max_distance <- mean(point$distance) + sd(point$distance)*nSigma
    logic_sub <- point$distance <= max_distance
    results <- cloud[logic_sub == TRUE, ]

  }

  if(method == "min_neighbors") {

    neighbors <- radius_search(cloud, cloud, radius, max_neighbour = (min_neighbours+1), same = TRUE, threads = threads, verbose = verbose, progress = progress, ...)
    count <- neighbors[, .N, by = query]
    count <- count[N >= min_neighbours,]
    results <- cloud[count$query, ]
  }

  if(method == "voxel_center") {

    center <- voxels(cloud, edge_length = c(edge_length, edge_length, edge_length), obj.voxels = FALSE)
    point <- knn(center[, 1:3], cloud, k = 1, same = FALSE, distance = distance, verbose = verbose, progress = progress)
    results <- cloud[point$ref, ]
  }

  return(results)

}
