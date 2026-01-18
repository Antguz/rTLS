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
#' @param ... Parameters passed to \code{\link[data.table:fread]{fread}} for the reading of \code{files}.
#'
#'
#' @return A \code{list} which contain a \code{data.table} (Trees) with the information of the point clouds used and their current coordinates in the stand, and another \code{data.table} with that compile all the point clouds used.
#' @author J. Antonio Guzmán Q.
#'
#' @importFrom sf st_polygon st_sfc st_area st_union st_difference st_intersection st_sample st_coordinates st_is_empty st_make_valid
#' @importFrom data.table data.table fread
#' @importFrom stats runif na.exclude
#' @importFrom utils txtProgressBar setTxtProgressBar
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
artificial_stand <- function(files,
                             n.trees,
                             dimension,
                             coordinates = NULL,
                             sample = TRUE,
                             replace = TRUE,
                             overlap = NULL,
                             rotation = TRUE,
                             degrees = NULL,
                             n_attempts = 100,
                             progress = TRUE,
                             plot = TRUE,
                             ...) {

  # ---- Basic checks ----
  if (length(files) < n.trees) {
    if (sample == FALSE) stop("The number of files selected is lower than n.trees")
    if (replace == FALSE) stop("The number of files selected without replacement is lower than n.trees")
  }

  if (!is.null(coordinates)) {
    if (ncol(coordinates) != 2) stop("The ncol of the coordinates differ from 2")
    if (nrow(coordinates) != n.trees) stop("The nrow of coordinates differ from the n.trees")
    if (!is.null(overlap)) stop("The overlap of tree crowns can not be controlled using established coordinates, select overlap = NULL")
    colnames(coordinates) <- c("X", "Y")
  }

  if (rotation) {
    if (is.null(degrees)) {
      degrees <- stats::runif(n.trees, 0.0, 360.0)
    } else {
      if (length(degrees) != n.trees) stop("The length of degrees differ from n.trees")
    }
  }

  # ---- File order ----
  if (sample) {
    ord <- base::sample(seq_along(files), n.trees, replace = replace)
    filestoread <- files[ord]
  } else {
    filestoread <- files
  }

  # ---- Stand boundary polygon (sf) ----
  # ring must be closed
  plotXY <- matrix(
    c(0, 0,
      dimension[1], 0,
      dimension[1], dimension[2],
      0, dimension[2],
      0, 0),
    ncol = 2, byrow = TRUE
  )

  spatial_plotXY <- sf::st_sfc(sf::st_polygon(list(plotXY)))
  spatial_plotXY <- sf::st_make_valid(spatial_plotXY)

  if (plot) {
    graphics::plot(spatial_plotXY, col = "lightgoldenrod", reset = FALSE)
  }

  # ---- Storage ----
  stant <- NULL                        # combined point cloud
  spatial_stant <- NULL                # union of crowns (sf geometry)
  available_space <- spatial_plotXY    # available placement area

  tcoordinates <- data.table::data.table(
    Tree = 1:n.trees,
    file = filestoread,
    Xcoordinate = NA_real_,
    Ycoordinate = NA_real_,
    CA = NA_real_,
    Hmax = NA_real_
  )

  # ---- Progress ----
  if (progress) {
    cat(paste0(
      " Creating an artificial forest stand of ",
      round(dimension[1], 2), " x ", round(dimension[2], 2),
      " with ", n.trees, " trees"
    ))
    pb <- utils::txtProgressBar(min = 0, max = n.trees, style = 3)
  }

  # ---- Helper: safe area numeric ----
  area_num <- function(g) {
    if (is.null(g)) return(0)
    if (length(g) == 0) return(0)
    # st_area returns units if CRS is set; here CRS is NA, but as.numeric is safe
    as.numeric(sf::st_area(g))
  }

  # ---- Main loop ----
  for (i in 1:n.trees) {

    if (progress) utils::setTxtProgressBar(pb, i)

    # Read point cloud
    tree <- data.table::fread(filestoread[i], ...)
    colnames(tree) <- c("X", "Y", "Z")
    tree$Z <- tree$Z - min(tree$Z)

    # Move to base centroid
    basetree <- subset(tree, Z >= 0 & Z <= 0.1)
    centroidXY <- c(mean(basetree$X), mean(basetree$Y))
    tree$X <- tree$X - centroidXY[1]
    tree$Y <- tree$Y - centroidXY[2]

    # Optional rotation (your existing function)
    if (rotation) {
      tree <- rotate3D(tree, roll = 0, pitch = 0, yaw = degrees[i])
    }

    # ---- Choose coordinates ----
    if (!is.null(coordinates)) {
      treecoordinates <- c(coordinates$X[i], coordinates$Y[i])
    } else {
      # sample within available_space (sf)
      # st_sample may return empty if geometry is empty
      samp <- sf::st_sample(available_space, size = 1, type = "random")
      if (length(samp) == 0 || sf::st_is_empty(samp)) {
        stop("No available space to sample a new tree location.", call. = FALSE)
      }
      xy <- sf::st_coordinates(samp)
      treecoordinates <- c(xy[1, 1], xy[1, 2])
    }

    # Apply translation
    tree_try <- tree
    tree_try$X <- tree_try$X + treecoordinates[1]
    tree_try$Y <- tree_try$Y + treecoordinates[2]

    basetree2 <- subset(tree_try, Z >= 0 & Z <= 0.1)
    newcentroidXY <- c(mean(basetree2$X), mean(basetree2$Y))

    # Build crown polygon from convex hull in XY
    ch <- grDevices::chull(tree_try[, 1:2])
    crown <- as.matrix(tree_try[ch, 1:2])
    crown <- rbind(crown, crown[1, ])  # close ring

    spatial_crown <- sf::st_sfc(sf::st_polygon(list(crown)))
    spatial_crown <- sf::st_make_valid(spatial_crown)

    # ---- First tree: accept directly (no overlap constraints needed) ----
    if (i == 1) {
      spatial_stant <- spatial_crown
      available_space <- sf::st_difference(spatial_plotXY, spatial_stant)
      available_space <- sf::st_make_valid(available_space)

      tree_try$Tree <- i
      stant <- tree_try

      if (plot) {
        graphics::plot(spatial_crown, col = "forestgreen", add = TRUE)
        graphics::points(newcentroidXY[1], newcentroidXY[2], col = "red")
      }

      tcoordinates$Xcoordinate[i] <- newcentroidXY[1]
      tcoordinates$Ycoordinate[i] <- newcentroidXY[2]
      tcoordinates$CA[i] <- area_num(spatial_crown)
      tcoordinates$Hmax[i] <- max(tree_try$Z)

      next
    }

    # ---- Other trees: retry until overlap criterion or attempts exceeded ----
    tries <- 1
    repeat {

      if (!is.null(coordinates)) {
        treecoordinates <- c(coordinates$X[i], coordinates$Y[i])
      } else {
        samp <- sf::st_sample(available_space, size = 1, type = "random")
        if (length(samp) == 0 || sf::st_is_empty(samp)) {
          # treat as a failed attempt; maybe available_space is fragmented
          tries <- tries + 1
          if (tries > n_attempts) {
            stop(
              "artificial_stand stopped: could not sample new coordinates (available space empty/invalid). ",
              "Try reducing overlap or n.trees, or increasing stand dimension.",
              call. = FALSE
            )
          }
          next
        }
        xy <- sf::st_coordinates(samp)
        treecoordinates <- c(xy[1, 1], xy[1, 2])
      }

      tree_try <- tree
      tree_try$X <- tree_try$X + treecoordinates[1]
      tree_try$Y <- tree_try$Y + treecoordinates[2]

      basetree2 <- subset(tree_try, Z >= 0 & Z <= 0.1)
      newcentroidXY <- c(mean(basetree2$X), mean(basetree2$Y))

      ch <- grDevices::chull(tree_try[, 1:2])
      crown <- as.matrix(tree_try[ch, 1:2])
      crown <- rbind(crown, crown[1, ])

      spatial_crown <- sf::st_sfc(sf::st_polygon(list(crown)))
      spatial_crown <- sf::st_make_valid(spatial_crown)

      A_crown <- area_num(spatial_crown)

      # Compute overlap % as intersection area / crown area
      if (is.null(overlap)) {
        ok <- TRUE
      } else {
        inter <- suppressWarnings(sf::st_intersection(spatial_crown, spatial_stant))
        A_inter <- area_num(inter)
        percentage <- if (A_crown > 0) (A_inter / A_crown) * 100 else Inf
        ok <- is.finite(percentage) && (percentage <= overlap)
      }

      tries <- tries + 1

      if (ok) {

        if (plot) {
          graphics::plot(spatial_crown, col = "forestgreen", add = TRUE)
          graphics::points(newcentroidXY[1], newcentroidXY[2], col = "red")
        }

        tree_try$Tree <- i
        stant <- rbind(stant, tree_try)

        spatial_stant <- sf::st_union(spatial_stant, spatial_crown)
        spatial_stant <- sf::st_make_valid(spatial_stant)

        available_space <- sf::st_difference(spatial_plotXY, spatial_stant)
        available_space <- sf::st_make_valid(available_space)

        tcoordinates$Xcoordinate[i] <- newcentroidXY[1]
        tcoordinates$Ycoordinate[i] <- newcentroidXY[2]
        tcoordinates$CA[i] <- A_crown
        tcoordinates$Hmax[i] <- max(tree_try$Z)

        break
      }

      if (tries > n_attempts) {
        stop(
          "artificial_stand was stopped because n_attempts was exceeded. ",
          "Try again and/or reduce overlap or n.trees, or increase stand dimension.",
          call. = FALSE
        )
      }
    }
  }

  # ---- Stand summary ----
  stand <- data.table::data.table(
    n.trees = n.trees,
    stand_area = (dimension[1] * dimension[2]),
    covered_area = area_num(spatial_plotXY) - area_num(available_space),
    total_crown_area = area_num(spatial_stant),
    n_points = if (is.null(stant)) 0 else nrow(stant)
  )

  final <- list(
    Stand = stand,
    Trees = stats::na.exclude(tcoordinates),
    Cloud = stant
  )

  return(final)
}
