neighborhood <- function(cloud, cloud_b = NULL, method, radius, k, parallel = FALSE, cores = NULL) {

  if(is.null(cloud_b) == TRUE) { #Selecting the cloud to calculated the neighborhood
    cloud_b <- cloud
  }

  cloud <- cloud[, 1:3] #Specify parameters as inputs.
  cloud_b <- cloud_b[, 1:3]

  colnames(cloud) <- c("X", "Y", "Z") #Change names of columns
  colnames(cloud_b) <- c("X", "Y", "Z")

  if(parallel == FALSE) {

    if(method == "sphere") {  #Method sphere

      print("Calculating spheres around points")
      pb <- txtProgressBar(min = 0, max = nrow(cloud), style = 3) #Set progress bar

      results <- cloud[, {setTxtProgressBar(pb, .GRP) ; sphere_neighbors(.SD, cloud_b, radius)}, by = seq_len(nrow(cloud))]
      colnames(results) <- c("points", "X", "Y", "Z", "distance")
      parameter <- radius
      names(parameter) <- "radius"

    } else if(method == "knn") { #Method knn

      print("Calculating knn")
      pb <- txtProgressBar(min = 0, max = nrow(cloud), style = 3) #Set progress bar

      results <- cloud[, {setTxtProgressBar(pb, .GRP) ; knn_neighbors(.SD, cloud_b, k, radius)}, by = seq_len(nrow(cloud))]
      colnames(results) <- c("points", "X", "Y", "Z", "distance")
      parameter <- k
      names(parameter) <- "k"

    }

  } else if(parallel == TRUE) {

    if(is.null(cores) == TRUE) {
      stop("Select the number of cores")
    }

    cl <- makeCluster(cores, outfile="")
    registerDoSNOW(cl)

    if(method == "sphere") {  #Method sphere

      print("Calculating spheres around points")
      pb <- txtProgressBar(min = 0, max = nrow(cloud), style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress=progress)

      results <- foreach(i = 1:nrow(cloud), .inorder = FALSE, .combine= rbind, .packages = c("data.table", "rTLS"), .options.snow = opts) %dopar% {
        results <- sphere_neighbors(x = cloud[i,], cloud = cloud_b, radius = radius)
        results$points <- i
        return(results[, c(5, 1:4)])
      }

      parameter <- radius
      names(parameter) <- "radius"

    } else if(method == "knn") { #Method knn

      print("Calculating knn")
      pb <- txtProgressBar(min = 0, max = nrow(cloud), style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress=progress)

      results <- foreach(i = 1:nrow(cloud), .inorder = FALSE, .combine= rbind, .packages = c("data.table", "rTLS"), .options.snow = opts) %dopar% {
        results <- knn_neighbors(x = cloud[i,], cloud = cloud_b, k= k, radius = radius)
        results$points <- i
        return(results[, c(5, 1:4)])
      }

      parameter <- k
      names(parameter) <- "k"

    }
      close(pb)
      stopCluster(cl)
  }

  final <- list(cloud = cloud, parameter = parameter, neighborhood = results)

  class(final) <- "neighborhood"
  return(final)
}

data(pc_tree)
cloud <- pc_tree[1:1000,]
cloud_b <- pc_tree

neighborhood(cloud, cloud_b = NULL, method = "sphere", radius = 0.2, parallel = TRUE, cores = 4)
