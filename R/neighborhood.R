neighborhood <- function(cloud, method, radius, k, parallel, ...) {
  
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