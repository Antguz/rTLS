#' @title Artificial forest stands
#'
#' @description Create artificial forest stands of a given area using tree point clouds.
#'
#' @param trees A \code{character} vector describing the file name or path of the tree point cloud to use.
#' @param n.trees A positive \code{numeric} vector describing the number of tree point cloud to use.
#' @param dimension A positive \code{numeric} vector o length two describing the width and length of the future forest stand.
#' @param sample Logical. If \code{TRUE}, it perform a sample of the \code{files} to determine the order to build the artificial stand. If \code{FALSE}, it use the file order described in \code{files}. \code{TRUE} as default.
#' @param replace Logical. If \code{TRUE}, it perform a sample selection with replacement to determine the order to build the artificial stand. \code{TRUE} as default. \code{TRUE} as default
#' @param overlap A positive \code{numeric} vector between 0 and 100 describing minimun value of overlap of the tree crowns in the future forest stand. If \code{NULL}, the degree of overlap is not controlled.
#' @param rotation Logical. If \code{TRUE}, it perform a random rotation in the x and y axis of the point cloud. \code{TRUE} as default.
#'
#' @return
#' @author J. Antonio Guzman Q.
#'
#' @examples
#'
#'
#' @export

artificial_stands <- function(files, n.trees, dimension, sample = TRUE, replace = TRUE, overlap = NULL, rotation = TRUE, degrees = NULL, plot = TRUE, ...) {

  if(length(files) < n.trees & replace == FALSE) { ###Asumtion on the number of files to use and trees selected.
    stop("The number of tress to use without replacement is lower than the number of files")
  }

  if(rotation == TRUE) {
    if(is.null(degrees) == TRUE) {
      degrees <- runif(n.trees, 0.0, 360.0)
    } else if(length(degrees) != n.trees) {
      stop("Rotation angles need to be provided for each n.trees")
    }
  }

  if(is.true(sample) == TRUE) {
    order <- sample(1:n.trees, replace = replace)
    trees <- files[order]
  }

  stant <- data.table(X = NA, Y = NA, Z = NA, files)

  if(plot == TRUE) { ####If plot obtion is truee
    plotXY <- matrix(c(0, 0, dimension[1], 0, dimension[1], dimension[2], 0, dimension[2], 0 , 0), ncol = 2, byrow = TRUE)
    p_plotXY <- Polygon(plotXY)
    spatial_plotXY <- SpatialPolygons(list(Polygons(list(p_plotXY), ID = "plot")))
    plot(spatial_plotXY)

    xpcoordinates <- c(0, 0, dimension[1], dimension[1])
    ypcoordinates <- c(0, dimension[2], 0, dimension[2])
    plotXY <- cbind(xpcoordinates, ypcoordinates)
    plotXY <- Polygon(cbind(xpcoordinates, ypcoordinates))
    ps_plotXY <- Polygons(list(plotXY),1)
    spatial_plotXY <- SpatialPolygons(list(ps_plotXY))
    plot(spatial_plotXY)
  }

  stant <- data.table(X = NA, Y = NA, Z = NA, files = NA) ###Final stant to create
  spatial_crown <- NA

  results <- foreach(i = 1:n.trees, .inorder = TRUE, .combine= rbind, .packages = c("data.table", "rTLS"), .options.snow = opts) %dopar% {  ####Conduct the loop

    ###Reading of the files-------------------------------------------------------------------

    tree <- fread(files[i], sep = "\t")
    colnames(tree) <- c("X", "Y", "Z")
    tree$Z <- tree$Z - min(tree$Z)

    ###Positioning in the plot and rotation of the clouds--------------------------------------

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


      if(i == 1) {  ###Dealing with the first tree ---------------------------------------------
        treecoordinates <- c(runif(1, 0, dimension[1]), runif(1, 0, dimension[2]))  ####Move the tree to their new position
        tree$X <- tree$X + treecoordinates[1]
        tree$Y <- tree$Y + treecoordinates[2]
        basetree <- subset(tree, Z >= 0 & Z <= 0.1)
        newcentroidXY <- c(mean(basetree$X), mean(basetree$Y))

        ch <- chull(tree[,1:2]) ###Crown in their space XY space
        crown <- tree[ch, 1:2]
        p_crown <- Polygon(crown)
        ps_crown <- Polygons(list(p_crown),1)
        spatial_crown <- SpatialPolygons(list(ps_crown))

        tree$files <- files[i]
        stant <- rbind(stant, tree)

        if(plot == TRUE) {
          plot(spatial_crown, add = TRUE)
          points(newcentroidXY[1], newcentroidXY[2], col = "red")
        }
      }

      if(i >= 2) {  ###Dealing with other trees ---------------------------------------------
        repeat {



          if(x == x) {
            break
          }
        }
      }


  }


}
