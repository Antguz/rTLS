#' Artificial Forest Stand
#'
#' Create an artificial forest stand of a given area using tree point clouds.
#'
#' When \code{coordinates = NULL}, \code{artifical_stand} adds, in sequence,
#' random coordinates to each \code{files} in the future stand based on the
#' crown area \code{overlap}. That is, first a tree from \code{files} is
#' randomly located within the stand \code{dimention}, then a second tree from
#' \code{files} will be located in the future stand based on the crown area
#' \code{overlap} from the previous tree, and so on. If during the random
#' location a given tree does not meet the requirements of \code{overlap}, new
#' random coordinates will be provided until the requirements are met.
#'
#' Since \code{artificial_stand} will try to add tree to the stand until the
#' requirements are met, this could lead to an infinite loop if the stand
#' \code{dimention} is small or if the trees on \code{files} are large or many
#' \code{n.trees}. Therefore, the use of \code{n_attempts} is recommended to avoid
#' this scenario.
#'
#' @param files A \code{character} vector describing the file name or path of
#'   the tree point cloud to use. Those files most contain three columns
#'   representing the *XYZ* coordinates of a given point cloud.
#' @param n.trees A positive \code{numeric} vector describing the number of
#'   point clouds to use.
#' @param dimension A positive \code{numeric} vector of length two describing
#'   the width and length of the future forest stand.
#' @param coordinates A \code{data.table} of two columns and with \code{nrows}
#'   equal to \code{n.trees} describing the basal *XYZ* coordinates of the point
#'   clouds in the future stand. If \code{NULL}, it uses random basal
#'   coordinates based on stand dimension. \code{NULL} as default.
#' @param sample Logical. If \code{TRUE}, it performs a sample of the \code{files} to determine the order to build the artificial stand. If \code{FALSE}, it use the file order described in \code{files}. \code{TRUE} as default.
#' @param replace Logical. If \code{TRUE}, it performs a sample selection with a replacement if \code{sample = TRUE} to determine the order to build the artificial stand. Useful if the \code{n.trees} is lower than \code{length(files)}. \code{TRUE} as default.
#' @param overlap A positive \code{numeric} vector between 0 and 100 describing the overlap percentage of a given the tree crowns in the future forest stand. If \code{NULL}, the degree of overlap is not controlled.
#' @param rotation Logical. If \code{TRUE}, it performs a rotation in yaw axis of the point cloud. \code{TRUE} as default.
#' @param degrees A positive \code{numeric} vector describing the degrees of rotation of the point clouds in the future stand. The \code{length(degree)} should be the same as \code{n.trees}. If \code{NULL}, it creates random degrees of rotation for each \code{n.trees}.
#' @param progress Logical, if \code{TRUE} displays a graphical progress bar. \code{TRUE} as default.
#' @param plot Logical. If \code{TRUE}, it provides visual tracking of the distribution of each tree in the artificial stand. This can not be exported as a return object.
#' @param n_attempts A positive \code{numeric} vector of length one describing the number of attempts to provide random \code{coordinates} until a tree met the \code{overlap} criteria.
#' This needs to be used if \code{coordinate = NULL} and \code{overlap != NULL}. \code{n_attempts = 100} as default.
#' @param ... Parameters passed to \code{\link{fread}} for the reading of \code{files}.
#'
#'
#' @return A \code{list} which contain a \code{data.table} (Trees) with the information of the point clouds used and their current coordinates in the stand, and another \code{data.table} with that compile all the point clouds used.
#' @author J. Antonio Guzmán Q.
#'
#' @importFrom sp Polygon
#' @importFrom sp Polygons
#' @importFrom sp SpatialPolygons
#' @importFrom sp plot
#' @importFrom sp spsample
#' @importFrom rgeos gArea
#' @importFrom rgeos gUnion
#' @importFrom rgeos gDifference
#' @importFrom foreach foreach
#' @importFrom foreach %do%
#' @importFrom stats runif
#' @importFrom stats na.exclude
#' @importFrom data.table data.table
#' @importFrom data.table fread
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom grDevices chull
#' @importFrom graphics points
#' @useDynLib rTLS, .registration = TRUE
#'
#' @seealso \code{\link{voxels_counting}}
#'
#' @examples
#' #' #Import an example point cloud
#' path <- system.file("extdata", "pc_tree.txt", package = "rTLS")
#'
#' #Creates a stand of 4 trees with 10% of overlap
#' files <- rep(path, 4)
#' artificial_stand(files, n.trees = 4, dimension = c(15, 15), overlap = 10)
#'
#' #Creates a stand of 4 trees with their locations
#' location <- data.table(X = c(5, 10, 10, 5), Y = c(5, 5, 10, 10))
#' artificial_stand(files, n.trees = 4, dimension = c(15, 15), coordinates = location)
#'
#'
#' @export
artificial_stand <- function(files, n.trees, dimension, coordinates = NULL, sample = TRUE, replace = TRUE, overlap = NULL, rotation = TRUE, degrees = NULL, n_attempts = 100, progress = TRUE, plot = TRUE, ...) {

  ####Posible errors or assumtions ------------------------------------------------------------------------------

  if(length(files) < n.trees) {
    if(sample == FALSE) {
      stop("The number of files selected is lower than n.trees")
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
    } else {
      if(length(degrees) != n.trees) {
        stop("The length of degrees differ from n.trees")
      }
    }
  }

  ###Selecting the order of files and files to use and their storage------------------------------------------------

  if(sample == TRUE) { ###Selecting the order of files and files to use
    order <- sample(1:length(files), n.trees, replace = replace)
    filestoread <- files[order]
  } else {
    filestoread <- files
  }

  stant <- NA ###Final stand to create
  spatial_stant <- NA
  tcoordinates <- data.table(Tree = 1:n.trees, file = filestoread, Xcoordinate = NA, Ycoordinate = NA, CA = NA, Hmax = NA)

  ###For plot estimations
  plotXY <- matrix(c(0, 0, dimension[1], 0, dimension[1], dimension[2], 0, dimension[2], 0 , 0), ncol = 2, byrow = TRUE)
  p_plotXY <- Polygon(plotXY)
  spatial_plotXY <- SpatialPolygons(list(Polygons(list(p_plotXY), ID = "plot")))

  if(plot == TRUE) { ####If plot option is true
    plot(spatial_plotXY, col = "lightgoldenrod")
  }

  ####Creating the loop for the artificial forest stand--------------------------------------------------------------

  if(progress == TRUE) {
    cat(paste("", "Creating an artificial forest stand of ", round(dimension[1], 2), " x ", round(dimension[2], 2), " with ", n.trees, " trees", sep = ""))  #Progress bar
    pb <- txtProgressBar(min = 0, max = n.trees, style = 3)
  }

  for(i in 1:n.trees) {  ####Conduct the loop

    if(progress == TRUE) {
      setTxtProgressBar(pb, i)
    }

    ###Reading of the files-------------------------------------
    tree <- fread(filestoread[i], ...)
    colnames(tree) <- c("X", "Y", "Z")
    tree$Z <- tree$Z - min(tree$Z)

    ###Positioning in the plot and rotation of the clouds-------
    basetree <- subset(tree, Z >= 0 & Z <= 0.1) ####Move the tree to their base centroid
    centroidXY <- c(mean(basetree$X), mean(basetree$Y))

    tree$X <- tree$X - centroidXY[1]
    tree$Y <- tree$Y - centroidXY[2]

    if(rotation == TRUE) {  ###If rotation occur
      tree <- rotate3D(tree, roll = 0, pitch = 0, yaw = degrees[i])
    }

    if(i == 1) {  ###Dealing with the first tree ----------------

      if(is.null(coordinates) != TRUE) { ####Move the tree to their new position
        colnames(coordinates) <- c("X", "Y")
        treecoordinates <- c(coordinates$X[i], coordinates$Y[i])
      } else {
        xy <- as.data.frame(spsample(spatial_plotXY, 1, type = "random")) #Set the random new coordinates
        treecoordinates <- c(xy[1,1], xy[1,2])
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
      available_space <- gDifference(spatial_plotXY, spatial_stant)

      tree$Tree <- i
      stant <- tree

      if(plot == TRUE) {
        plot(spatial_stant, col = "forestgreen", add = TRUE)
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
          xy <- as.data.frame(spsample(available_space, 1, type = "random")) #Set the random new coordinates
          treecoordinates <- c(xy[1,1], xy[1,2])
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

        try <- try + 1

        if(is.null(overlap) == TRUE) {
          if(plot == TRUE) {
            plot(spatial_crown, col = "forestgreen", add = TRUE)
            points(newcentroidXY[1], newcentroidXY[2], col = "red")
          }

          tree_try$Tree <- i
          stant <- rbind(stant, tree_try)

          spatial_stant <- gUnion(spatial_crown, spatial_stant)
          available_space <- gDifference(available_space, spatial_stant)

          tcoordinates$Xcoordinate[i] <- newcentroidXY[1]   ###Information of each tree for tcoordinates
          tcoordinates$Ycoordinate[i] <- newcentroidXY[2]
          tcoordinates$CA[i] <- gArea(spatial_crown)
          tcoordinates$Hmax[i] <- max(tree_try$Z)

          break
        } else if(percentage <= overlap) {

          if(plot == TRUE) {
            plot(spatial_crown, col = "forestgreen", add = TRUE)
            points(newcentroidXY[1], newcentroidXY[2], col = "red")
          }

          tree_try$Tree <- i
          stant <- rbind(stant, tree_try)

          spatial_stant <- gUnion(spatial_crown, spatial_stant)
          available_space <- gDifference(available_space, spatial_stant)

          tcoordinates$Xcoordinate[i] <- newcentroidXY[1]   ###Information of each tree for tcoordinates
          tcoordinates$Ycoordinate[i] <- newcentroidXY[2]
          tcoordinates$CA[i] <- gArea(spatial_crown)
          tcoordinates$Hmax[i] <- max(tree_try$Z)
          break
        } else if(try > n_attempts) {
          stop("artificial_stand was stopped due to the n_attempts exceeds the established number, Try it again and/or reduce the value of the parameters of overlap or n.trees", call. = FALSE)
        }
      }
    }
  }

  stand <- data.table(n.trees = n.trees,
                      stand_area = (dimension[1]*dimension[2]),
                      covered_area = (gArea(spatial_plotXY) - gArea(available_space)),
                      total_crown_area = gArea(spatial_stant),
                      n_points = nrow(stant))

  final <- list(Stand = stand, Trees = na.exclude(tcoordinates), Cloud = stant)
  return(final)
}

