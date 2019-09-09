#' @title Random Point Cloud
#'
#' @description Creates a random point cloud using a given number of points, *XYZ* limints, and minimun distance between them.
#'
#' @param npoints A positive \code{numeric} vector of length 1 describing the number of points to create.
#' @param limits A \code{numeric} vector of length 6 representing the minimun and maximun of *XYZ* coordinates.
#' @param minDistance A positive \code{numeric} vector of length 1 describing the minimun distance between points.
#'
#' @details This function create points based on the established \code{limits} and \code{minDistance} between points. It is useful more than a random number generator since the user can have control of the minimum spatial resolution of the point cloud.
#'
#' @return A \code{data.table} whit three columns representing the *XYZ* coordinates.
#' @author J. Antonio Guzmán Q.
#'
#' @examples
#'
#' #Creates a random point cloud with a minimum distance of 0.5 and a range between 0 and 100.
#' range <- c(Xmin = 0, Xmax = 100, Ymin = 0, Ymas = 100, Zmin = 0, Zmax = 100)
#' random_cloud(npoints = 1000, limits =  range, minDistance = 0.5)
#'
#' @export
random_cloud <- function(npoints, limits, minDistance) {

  max_points <- as.numeric(length(seq(limits[1], limits[2], minDistance)))*as.numeric(length(seq(limits[3], limits[4], minDistance)))*as.numeric(length(seq(limits[5], limits[6], minDistance)))

  if(npoints > max_points) {
    stop(paste("", "It is not posible to create ", npoints, " points using the defined minDistance with these limits", sep = ""))
  }

  random <- data.table(X = runif(1, limits[1], limits[2]), #Create the first random point
                       Y = runif(1, limits[3], limits[4]),
                       Z = runif(1, limits[5], limits[6]))

  i <- 2

  print("Creating a random point cloud")
  pb <- txtProgressBar(min = 1, max = npoints, style = 3) #Set progress bar

  repeat{

    random_toadd <- data.table(X = runif(1, limits[1], limits[2]), #Start to creates the other points
                               Y = runif(1, limits[3], limits[4]),
                               Z = runif(1, limits[5], limits[6]))

    distance <- sqrt((random_toadd$X-random$X)^2 + (random_toadd$Y-random$Y)^2 + (random_toadd$Z-random$Z)^2) < minDistance # pythagorean theorem

    if(any(distance) == FALSE) { # If the random point meet the minDistance
      random <- rbind(random, random_toadd) #Add to the final random cloud
      i <- i + 1 # repeat the procedure with the next element
    }

    setTxtProgressBar(pb, i)

    if (i > npoints) { #If the number number of random points are enough
      break
    }
  }

  return(random)
}
