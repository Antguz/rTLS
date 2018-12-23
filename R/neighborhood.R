#' @import dplyr
#'
#' @title Neighboring points
#'
#' @description Estimate neighboring points based on two methods.
#'
#' @param cloud A  \code{matrix} or  \code{data.frame} with xyz coordinates in the first three columns.
#' @param method A character string specifying the method to estimated the neighbors. It most be one of  \code{"distance"} or  \code{"knn"}.
#' @param radius An integer of a length 1 representing the number of neighbors to consider. This will be used if  \code{method = "distance"}.
#' @param k An integer of a length 1 representing the number of neighbors to consider. This will be used if  \code{method = "knn"}.
#' @param parallel Logical, if  \code{TRUE} it use a parallel processing.
#'
#' @value An object of class "neighborhood" with a list per point of the neighboring points.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#' @references Wang, D., Hollaus, M., & Pfeifer, N. (2017). Feasibility of machine learning methods for separating wood and leaf points from Terrestrial Laser Scanning data. ISPRS Annals of Photogrammetry, Remote Sensing & Spatial Information Sciences, 4.
#'
#' @examples
#' data("pc_tree")
#'
#' ###Estimate the niegborhood based on distance
#' neighborhood(pc_tree, method = "distance", radius = 0.2)
#'
#' ###Estimate the niegborhood based on knn
#' neighborhood(pc_tree, method = "knn", k = 10)
#'
#'@export
neighborhood <- function(cloud, method, radius, k, parallel) {

  if(parallel == TRUE) { ###Parallel TRUE

    if(method == "distance") {  #Method distance
      pack <- list(.packages = c("dplyr", "bio3d"))
      results <- alply(cloud, .margins = 1, .fun = dist_neighbors, cloud = cloud, radius = radius, .progress = "text", .parallel = TRUE, .paropts = pack, .inform = FALSE)
    } else if(method == "knn") { #Method knn
      pack <- list(.packages = c("dplyr", "nabor"))
      results <- alply(cloud, .margins = 1, .fun = knn_neighbors, cloud = cloud, k = k, .progress = "text", .parallel = TRUE, .paropts = pack, .inform = FALSE)
    }

  } else if(parallel == FALSE) { ###Parallel FALSE

    if(method == "distance") {  #Method distance
      results <- alply(cloud, .margins = 1, .fun = dist_neighbors, cloud = cloud, radius = radius, .progress = "text", .inform = FALSE)
    } else if(method == "knn") { #Method knn
      results <- alply(cloud, .margins = 1, .fun = knn_neighbors, cloud = cloud, k = k, .progress = "text", .inform = FALSE)
    }
  }
  class(results) <- "neighborhood"
  results
}
