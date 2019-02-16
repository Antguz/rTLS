#' @title Waveforms derived from voxelization method
#'
#' @description Create waveforms based on the vertical axis using points or voxels derived from \code{voxels()}.
#'
#' @param voxels A object of class \code{"voxels"} using the function \code{voxels()} or a \code{data.table} with the coordinates of the voxels created and the number of points in each voxel.
#' @param voxel.size If \code{class(voxels) == "data.table"}, a positive \code{numeric} vector with the size of the voxels used. It use the same dimentional scale of the point cloud.
#' @param method Logical. If \code{TRUE}, it generates voxels on a set of random points created using the same number of points and _xyz_ range of \code{cloud}. \code{FALSE} as default.
#' @param values.perc Logical. If \code{TRUE}, it compute the vertical waveforms based on the percentages of points or voxels in each layer using the total of points of the cloud or the total of voxels created. \code{TRUE} as default.
#' @param height.perc Logical. If \code{TRUE}, it compute the vertical waveforms based on the percentages of the maximun (100) and minimun (0) height of the z axis. \code{TRUE} as default.
#' @param plot Logical. If \code{TRUE}, it returns a plot of the waveform. \code{TRUE} as default.
#'
#' @return
#'
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#' @references Greaves, H. E., Vierling, L. A., Eitel, J. U., Boelman, N. T., Magney, T. S., Prager, C. M., & Griffin, K. L. (2015). Estimating aboveground biomass and leaf area of low-stature Arctic shrubs with terrestrial LiDAR. Remote Sensing of Environment, 164, 26-35.
#'
#' @examples
#' data("pc_tree")
#'
#' ###Create voxels of a size of 0.5. and extract the number of voxels in the vertigal gradient
#' vox <- voxels(pc_tree, voxel.size = 0.5)
#' wavefrom(vox, method = "voxels", values.perc = TRUE, height.perc = TRUE)
#'
#' ###Returns just the coordinates of the voxels and the number of points in each voxel
#' voxels(pc_tree, voxel.size = 0.5, obj.voxel = FALSE)
#'
#' ###Create a random cloud based on a point cloud and return voxels
#' voxels(pc_tree, voxel.size = 0.5, random = TRUE)
#'
#' @export
wavefrom <- function(voxels, voxel.size = NULL, method, values.perc = TRUE, height.perc = TRUE, plot = TRUE) {

  if(class(voxels)[1] != "voxels") { ###Select  the vertical gradient and data.base if there is no a voxels class_________________________________________
    if(is.null(voxel.size) == TRUE) {
      stop("voxel.size need to be defined")
    }

    size <- voxel.size

    if(height.perc == TRUE) { ###Select height.percentages is true
      gradient <- (size-(min(voxels$voxels$Z)-size/2))/((max(voxels$voxels$Z)+size/2)-(min(voxels$voxels$Z)-size/2))

    } else { ###Select height.percentages is false
      gradient <- size
    }

    cubes <- voxels

  } else if(class(voxels)[1] == "voxels") { ###Select the vertical gradient if there is an object of class voxels

    size <- voxels$parameter

    if(height.perc == TRUE) { ###Select height.percentages is true
      gradient <- (size-(min(voxels$voxels$Z)-size/2))/((max(voxels$voxels$Z)+size/2)-(min(voxels$voxels$Z)-size/2))

    } else { ###Select height.percentages is false
      gradient <- voxel.size
    }

    cubes <- voxels$voxels
  }

  if(height.perc == TRUE) {  ###Select representation for height if percentage is true________________________________________________________
    height <- cubes[ , .N, by = .(Z)][,1]
    height <- (height$Z-(min(height$Z)-size/2))/((max(height$Z)+size/2)-(min(height$Z)-size/2))*100
    x.names <- "Height (%)"

  } else if(height.perc == FALSE) { ###Select the representation for height if percentage is false
    height <- cubes[ , .N, by = .(Z)][,1]
    x.names <- "Height"
  }

  if(method == "voxels") {  ###If the method voxels is selected_______________________________________________________________________________
    if(values.perc == TRUE) { #If the representation of voxels in percentages is true
      total <- nrow(cubes)*(size^3)
      values <- (((cubes[ , .N, by = .(Z)][,2])*(size^3))/total)*100
      y.names <- "Volumen (%)"

    } else if (values.perc == FALSE) { #If the representation of voxels in percentages is false
      values <- (cubes[ , .N, by = .(Z)][,2])*(size^3)
      y.names <- "Volumen"
    }

  } else if(method == "points") {

    if(values.perc == TRUE) { #If the representation of points in percentages is true
      total <- sum(cubes$N)
      values <- cubes[, N := sum(N), by = c("Z")]
      values <- (unique(cubes$N)/total)*100
      y.names <- "Number of points (%)"

    } else if (values.perc == FALSE) { #If the representation of points in percentages is false
      values <- unique(cubes$N)
      y.names <- "Number of points"
    }
  }

  frame <- data.table(height, values)
  colnames(frame) <- c(x.names, y.names)

  return(frame)
}

