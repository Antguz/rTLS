#' @import dplyr
#'
#' @title
#'
#' @description
#'
#' @param data
#' @param parallel
#' @param method
#' @param radius
#' @param k
#'
#'@export
cloud_dimensionality <- function(data, parallel, method, radius, k) {

  if(class(data) == "neighborhood") {  ####For objects of class "neighborhood"

    if(parallel == TRUE) { ###Parallel TRUE
      results <- ldply(data, .fun = dimensionality,
                       .progress = "text", .parallel = TRUE, .inform = FALSE)
      results
    } else if(parallel == FALSE) {  ###Parallel FALSE
      results <- ldply(data, .fun = dimensionality,
                       .progress = "text", .parallel = FALSE, .inform = FALSE)
      results
    }
  } else {   ####For objects of class data.frame or matrix

    if(parallel == TRUE) { ###Parallel TRUE

      if(method == "distance") {  #Method distance
        pack <- list(.packages = c("dplyr", "bio3d"))
        results <- adply(cloud, .margins = 1, .fun = dist_dimensionality, cloud = cloud, radius = radius, .progress = "text", .parallel = TRUE, .paropts = pack, .inform = FALSE)
      } else if(method == "knn") { #Method knn
        pack <- list(.packages = c("dplyr", "nabor"))
        results <- adply(cloud, .margins = 1, .fun = knn_dimensionality, cloud = cloud, k = k, .progress = "text", .parallel = TRUE, .paropts = pack, .inform = FALSE)
      }

    } else if(parallel == FALSE) { ###Parallel FALSE

      if(method == "distance") {  #Method distance
        results <- adply(cloud, .margins = 1, .fun = dist_dimensionality, cloud = cloud, radius = radius, .progress = "text", .inform = FALSE)
      } else if(method == "knn") { #Method knn
        results <- adply(cloud, .margins = 1, .fun = knn_dimensionality, cloud = cloud, k = k, .progress = "text", .inform = FALSE)
      }
    }
    results
  }
}
