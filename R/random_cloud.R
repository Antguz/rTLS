#' @title Random point cloud
#'
#' @description Creates a random point cloud using a given number of points, xyz limints, and minimun distance between them.
#'
#' @param npoints A positive \code{numeric} vector of length 1 describing the number of points to create.
#' @param limits A \code{numeric} vector of length 6 representing the minimun and maximun of x, y, and z coordinates.
#' @param minDistance A positive \code{numeric} vector of length 1 describing the future minimun distance between points.
#'
#' @return A \code{data.table} whit three columns representing the xyz coordinates.
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#'
#' @examples
#'
#' ###Create a random point cloud of 1000 points with a minumun distance of 0.5 and using a xyz range between 0 and 100.
#' range <- c(Xmin = 0, Xmax = 100, Ymin = 0, Ymas = 100, Zmin = 0, Zmax = 100)
#' random_cloud(npoints = 1000, limits =  range, minDistance = 0.5)
#'
#' @export
random_cloud <- function(npoints, limits, minDistance) {

  max_points <- length(seq(limits[1], limits[2], minDistance))*length(seq(limits[3], limits[4], minDistance))*length(seq(limits[5], limits[6], minDistance))

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
