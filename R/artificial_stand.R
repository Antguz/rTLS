#' @title Artificial Forest Stands
#'
#' @description Create an artificial forest stand of a given area using tree point clouds.
#'
#' @param files A \code{character} vector describing the file name or path of the tree point cloud to use. Those files most contain three columns representing the *XYZ* coordinates of a given point cloud.
#' @param n.trees A positive \code{numeric} vector describing the number of point clouds to use.
#' @param dimension A positive \code{numeric} vector of length two describing the width and length of the future forest stand.
#' @param coordinates A \code{data.table} of two columns and with \code{nrows} equal to \code{n.trees} describing the basal *XYZ* coordinates of the point clouds in the future stand. If \code{NULL}, it uses random basal coordinates based on stand dimension. \code{NULL} as default.
#' @param sample Logical. If \code{TRUE}, it performs a sample of the \code{files} to determine the order to build the artificial stand. If \code{FALSE}, it use the file order described in \code{files}. \code{TRUE} as default.
#' @param replace Logical. If \code{TRUE}, it performs a sample selection with a replacement if \code{sample = TRUE} to determine the order to build the artificial stand. Useful if the \code{n.trees} is lower than \code{length(files)}. \code{TRUE} as default.
#' @param overlap A positive \code{numeric} vector between 0 and 100 describing the overlap percentage of a given the tree crowns in the future forest stand. If \code{NULL}, the degree of overlap is not controlled.
#' @param rotation Logical. If \code{TRUE}, it performs a random rotation in the x and y axis of the point cloud. \code{TRUE} as default.
#' @param degrees A positive \code{numeric} vector describing the degree or degrees of rotation of the point cloud in the future stand. The \code{length(degree)} should be the same as \code{n.trees}. If \code{NULL}, it creates random degrees of rotation for each \code{n.trees}.
#' @param plot Logical. If \code{TRUE}, it provides visual tracking of the distribution of each tree in the artificial stand. This can not be exported as a return object.
#' @param ... Parameters passed to \code{\link{fread}} for the reading of \code{files}.
#'
#' @return A \code{list} which contain a \code{data.table} (Trees) with the information of the point clouds used and their current coordinates in the stand, and another \code{data.table} with that compile all the point clouds used.
#' @author J. Antonio Guzmán Q.
#'
#' @importFrom sp Polygon
#' @importFrom sp Polygons
#' @importFrom sp SpatialPolygons
#' @importFrom sp plot
#' @importFrom rgeos gArea
#' @importFrom rgeos gUnion
#' @importFrom foreach foreach
#' @importFrom foreach %do%
#' @importFrom stats runif
#' @importFrom data.table data.table
#' @importFrom data.table fread
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom grDevices chull
#' @importFrom graphics points
#'
#' @seealso \code{\link{voxels_counting}}
#'
#' @examples
#' #Import an example point cloud
#' path <- system.file("extdata", "pc_tree.txt", package = "rTLS")
#'
#' ###Creates a stand of 15x15 repeating four times the same point cloud and random coordinates and a crown overlap of 10%
#' files <- rep(path, 4)
#' artificial_stands(files, n.trees = 4, dimension = c(15, 15), coordinates = NULL, sample = FALSE, replace = FALSE, overlap = 10, rotation = TRUE, degrees = NULL, plot = TRUE)
#'
#' ###Creates a stand of 15x15 repeating four times the same point cloud with establish locations.
#' location <- data.table(X = c(5, 10, 10, 5), Y = c(5, 5, 10, 10))
#' artificial_stands(files, n.trees = 4, dimension = c(15, 15), coordinates = location, sample = FALSE, replace = FALSE, overlap = NULL, rotation = TRUE, degrees = NULL, plot = TRUE)
#'
#' @export
artificial_stands <- function(files, n.trees, dimension, coordinates = NULL, sample = TRUE, replace = TRUE, overlap = 0, rotation = TRUE, degrees = NULL, plot = TRUE, ...) {

  ####Posible errors or assumtions ------------------------------------------------------------------------------

  if(length(files) < n.trees) {
    if(sample == FALSE) {
      stop("The number of files selected lower than n.trees")
    }
    if(replace == FALSE) {
      stop("The number of files selected without replacement is lower than n.trees")
    }
  }

  if(is.null(coordinates) != TRUE) {
    if(ncol(coordinates) != 2) {
      stop("The ncol of the coordinates differ from 2")
    }
    if(nrow(coordinates) != n.trees) {
      stop("The nrow of coordinates differ from the n.trees")
    }
    if(is.null(overlap) != TRUE) {
      stop("The overlap of tree crowns can not be controled using established coordinates, select overlap = NULL")
    }
  }

  if(rotation == TRUE) {
    if(is.null(degrees) == TRUE) {
      degrees <- runif(n.trees, 0.0, 360.0)
    } else if(length(degrees) != n.trees) {
      stop("The length of dregrees differ from n.trees")
    }
  }

  ###Selecting the order of files and files to use and their storage------------------------------------------------

  if(sample == TRUE) { ###Selecting the order of files and files to use
    order <- sample(1:length(files), n.trees, replace = replace)
    filestoread <- files[order]
  } else {
    filestoread <- files
  }

  stant <- NA ###Final stant to create
  spatial_stant <- NA
  tcoordinates <- data.table(files = filestoread, Xcoordinate = NA, Ycoordinate = NA, CA = NA, Hmax = NA)

  if(plot == TRUE) { ####If plot obtion is truee
    plotXY <- matrix(c(0, 0, dimension[1], 0, dimension[1], dimension[2], 0, dimension[2], 0 , 0), ncol = 2, byrow = TRUE)
    p_plotXY <- Polygon(plotXY)
    spatial_plotXY <- SpatialPolygons(list(Polygons(list(p_plotXY), ID = "plot")))
    plot(spatial_plotXY)
  }

  ####Creating the loop for the artificial forest stand--------------------------------------------------------------

  print(paste("", "Creating an artificial forest stand of ", dimension[1], "x", dimension[2], " with ", n.trees, " trees", sep = ""))  #Progress bar
  pb <- txtProgressBar(min = 0, max = length(filestoread), style = 3)

  results <- foreach(i = 1:n.trees, .inorder = TRUE, .combine= rbind, .packages = c("data.table", "sp", "rTLS", "rgeos")) %do% {  ####Conduct the loop

    setTxtProgressBar(pb, i)

    ###Reading of the files-------------------------------------

    tree <- fread(filestoread[i], ...)
    colnames(tree) <- c("X", "Y", "Z")
    tree$Z <- tree$Z - min(tree$Z)

    ###Positioning in the plot and rotation of the clouds-------

    if(is.null(rotation) == TRUE) {  ###If rotation ocur
      treeXY <- tree[,1:2]
      cosangle <- cos(degrees[i])
      sinangle <- sin(degrees[i])
      newXY <- as.matrix(treeXY) %*% t(matrix(c(cosangle, sinangle, -sinangle, cosangle), 2,2))
      tree$X <- newXY[,1]
      tree$Y <- newXY[,2]
    }

    basetree <- subset(tree, Z >= 0 & Z <= 0.1) ####Move the tree to their base centroid
    centroidXY <- c(mean(basetree$X), mean(basetree$Y))
    tree$X <- tree$X - centroidXY[1]
    tree$Y <- tree$Y - centroidXY[2]


    if(i == 1) {  ###Dealing with the first tree ----------------

      if(is.null(coordinates) != TRUE) { ####Move the tree to their new position
        colnames(coordinates) <- c("X", "Y")
        treecoordinates <- c(coordinates$X[i], coordinates$Y[i])
      } else {
        treecoordinates <- c(runif(1, 0, dimension[1]), runif(1, 0, dimension[2]))
      }

      tree$X <- tree$X + treecoordinates[1]
      tree$Y <- tree$Y + treecoordinates[2]
      basetree <- subset(tree, Z >= 0 & Z <= 0.1)
      newcentroidXY <- c(mean(basetree$X), mean(basetree$Y))

      ch <- chull(tree[,1:2]) ###Crown in their space XY space
      crown <- tree[ch, 1:2]
      p_crown <- Polygon(crown)
      ps_crown <- Polygons(list(p_crown), ID = as.character(i))
      spatial_stant <- SpatialPolygons(list(ps_crown))

      tree$files <- filestoread[i]
      stant <- tree

      if(plot == TRUE) {
        plot(spatial_stant, add = TRUE)
        points(newcentroidXY[1], newcentroidXY[2], col = "red")
      }

      tcoordinates$Xcoordinate[i] <- newcentroidXY[1]   ###Information of each tree for tcoordinates
      tcoordinates$Ycoordinate[i] <- newcentroidXY[2]
      tcoordinates$CA[i] <- gArea(spatial_stant)
      tcoordinates$Hmax[i] <- max(tree$Z)

    }

    if(i > 1) {  ###Dealing with other trees ----------------------

      try <- 1

      repeat {
        if(is.null(coordinates) != TRUE) { ####Move the tree to their new position
          colnames(coordinates) <- c("X", "Y")
          treecoordinates <- c(coordinates$X[i], coordinates$Y[i])
        } else {
          treecoordinates <- c(runif(1, 0, dimension[1]), runif(1, 0, dimension[2]))
        }

        tree_try <- tree
        tree_try$X <- tree_try$X + treecoordinates[1]
        tree_try$Y <- tree_try$Y + treecoordinates[2]
        basetree <- subset(tree_try, Z >= 0 & Z <= 0.1)
        newcentroidXY <- c(mean(basetree$X), mean(basetree$Y))

        ch <- chull(tree_try[,1:2]) ###Crown in their space XY space
        crown <- tree_try[ch, 1:2]
        p_crown <- Polygon(crown)
        ps_crown <- Polygons(list(p_crown), ID = i)
        spatial_crown <- SpatialPolygons(list(ps_crown))

        area_intercepted <- gArea(spatial_crown) - (gArea(gUnion(spatial_crown, spatial_stant)) - gArea(spatial_stant))
        percentage <- area_intercepted/gArea(spatial_crown)*100

        if(is.null(overlap) == TRUE) {
          if(plot == TRUE) {
            plot(spatial_crown, add = TRUE)
            points(newcentroidXY[1], newcentroidXY[2], col = "red")
          }

          tree_try$files <- filestoread[i]
          stant <- rbind(stant, tree_try)

          spatial_stant <- gUnion(spatial_crown, spatial_stant)

          tcoordinates$Xcoordinate[i] <- newcentroidXY[1]   ###Information of each tree for tcoordinates
          tcoordinates$Ycoordinate[i] <- newcentroidXY[2]
          tcoordinates$CA[i] <- gArea(spatial_crown)
          tcoordinates$Hmax[i] <- max(tree_try$Z)

          break
        } else if(percentage <= overlap) {

          if(plot == TRUE) {
            plot(spatial_crown, add = TRUE)
            points(newcentroidXY[1], newcentroidXY[2], col = "red")
          }

          tree_try$files <- filestoread[i]
          stant <- rbind(stant, tree_try)

          spatial_stant <- gUnion(spatial_crown, spatial_stant)

          tcoordinates$Xcoordinate[i] <- newcentroidXY[1]   ###Information of each tree for tcoordinates
          tcoordinates$Ycoordinate[i] <- newcentroidXY[2]
          tcoordinates$CA[i] <- gArea(spatial_crown)
          tcoordinates$Hmax[i] <- max(tree_try$Z)
          break
        }
        try <- try + 1
      }
    }
  }
  final <- list(Trees = tcoordinates, Cloud = stant)
  return(final)
}
