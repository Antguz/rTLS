#' @title Forest Structure
#'
#' @description Estimates the forest structure of discrete returns from a TLS scan.
#'
#' @param scan A \code{data.table} with three columns with describe *XYZ* coordinates of the discrete return of a TLS scan.
#' @param zenith.range A \code{numeric} vector of length two describing the \code{min} and \code{max} range of the zenith angle to use. Theoretically, the \code{max} range should be lower than 90 degrees.
#' @param zenith.bands A \code{numeric} vector of length one describing the number of bands to use between the \code{zenith.range} to estimate the frecuency of laser shots and measured points in \code{scan}.
#' @param azimuth.range A \code{numeric} vector of length two describing the range of the azimuth angle to use. Theoretically, it should be between 0 and 360 degrees.
#' @param TLS.resolution A \code{numeric} vector of length two describing the horizontal and vertical angle resolution.
#' @param TLS.coordinates A \code{numeric} vector of length three the scanner coordinates within \code{returns}.
#' If \code{NULL}, it assumes that the coordinates are \code{c(X = 0, Y = 0, Z = 0)}.
#' @param TLS.frame A \code{numeric} vector of length four describing the \code{min} and \code{max} of the zenith and azimuth angle of the scan frame.
#' If \code{NULL}, it assumes that a complete hemisphere: \code{c(zenith.min = 0, zenith.max = 90, azimuth.min = 0, azimuth.max = 360)}.
#' @param TLS.angles A \code{numeric} vector of length three describing the pitch (*X*), roll (*Y*), and yaw (*Z*) of the TLS during the scan.
#' If \code{NULL}, it assumes that the angles are \code{c(pitch = 0, roll = 0, yaw = 0)}.
#' @param parallel Logical, if \code{TRUE} it use parallel processing on the estimation of shots and returns. \code{FALSE} as default.
#' @param cores An \code{integer >= 0} describing the number of cores use. This need to be used if \code{parallel = TRUE}.
#' @details
#'
#' @return
#'
#' @author J. Antonio Guzmán Q.
#'
#' @importFrom
#' @importFrom
#' @importFrom
#'
#' @seealso \code{\link{}}
#'
#' @examples
#' scan <- fread("C:/Users/josea/Documents/Github/tmp/hemi.txt", sep = "\t")
#' zenith.range <- c(50.0000, 70.0000)
#' zenith.bands <- 5
#' azimuth.range <- c(0.0000, 360.0000)
#' TLS.resolution <- c(0.0400, 0.0400)
#' TLS.coordinates <- c(0, 0, 0)
#' TLS.frame <- c(30.0000, 150.0000, 0, 360.0000)
#' TLS.angles <- c(-0.60531428717225377, 1.3143536309079467, 172.09018411223335)
#'
#'
#'
#' @export
fstructure <- function(scan, zenith.range, zenith.bands, azimuth.range, TLS.resolution, TLS.coordinates = NULL, TLS.frame = NULL, TLS.angles = NULL, parallel = FALSE, cores = NULL) {

  colnames(scan)[1:3] <- c("X", "Y", "Z")

  ###Validate assumptions-------------------------------------------------------------------------------------------------------
  if(is.null(cores) == TRUE) {
    stop("Select the number of cores")
  }

  if(is.null(TLS.coordinates) == TRUE) {
    TLS.coordintates <- c(X = 0, Y = 0, Z = 0)
  } else if(length(TLS.coordinates) != 3) {
    stop("The length of TLS.coordinates needs to be three representing the *XYZ*")
  }

  if(is.null(TLS.frame) == TRUE) {
    TLS.frame <- c(zenith.min = 0, zenith.max = 90, azimuth.min = 0, azimuth.max = 360)
  } else if(length(TLS.frame) != 4) {
    stop("The length of the TLS.frame needs to be four representing the min and max of the zenith and azimuth TLS scan")
  }

  if(is.null(TLS.angles) == TRUE) {
    TLS.angles <- c(pitch = 0, roll = 0, azimuth = 0)
  } else if(length(TLS.angles) != 3) {
    stop("The length of the TLS.angles needs to be three representing the pitch, roll, and yaw of the TLS during the scan")
  }

  ###Estimates reaturns angles based on the TLS coordinates--------------------------------------------------------------------------------------

  scan[, h := Z - TLS.coordinates[3]]
  scan <- scan[h >= 0,]

  #Turn the scan based on the yaw
  if(TLS.angles[3] != 0) {
    scan <- move_rotate(scan, move = NULL, rotate = c(0, 0, TLS.angles[3]))
  }

  scan <- cbind(scan, cloud_angles(scan, TLS.coordinates))
  scan <- scan[between(zenith, zenith.range[1], zenith.range[2]),]

  ###Estimate the number of scanner pulses in a given zenith and azimuth range --------------------------------------------------------------

  model <- CJ(zenith = seq(TLS.frame[1], TLS.frame[2], 1),
              azimuth = seq(TLS.frame[3], TLS.frame[4], 1))

  model[ , c("X", "Y", "Z") := list((cos((azimuth*pi)/180) * sin((zenith*pi)/180)),
                                    (sin((azimuth*pi)/180) * sin((zenith*pi)/180)),
                                    (cos((zenith*pi)/180))), by = seq_len(nrow(model))]

  model[, 3:5] <- move_rotate(model[,3:5], move = NULL, rotate = c(TLS.angles[1], TLS.angles[2], TLS.angles[3]))
  model[, 1:2] <- cloud_angles(model[,3:5], NULL)

  new.frame <- c(min(model$zenith), max(model$zenith), min(model$azimuth), max(model$azimuth))

  scanner <- CJ(zenith = seq((new.frame[1] + (TLS.resolution[1]/2)), new.frame[2], TLS.resolution[1]),
                azimuth = seq((new.frame[3] + (TLS.resolution[1]/2)), new.frame[4], TLS.resolution[2]))

  scanner <- scanner[between(zenith, zenith.range[1], zenith.range[2]) , 1:2]

  ###Extraction of the structure metrics------------------------------
  #Set the table to return
  bands.limits <- seq(zenith.range[1], zenith.range[2], zenith.bands-1)

  bands <- data.table(rings = seq(1, zenith.bands, 1),
                      min.zenith = c(bands.limits[1:length(bands.limits)-1]),
                      max.zenith = c(bands.limits[2:length(bands.limits)]))

  frame <- CJ(rings = seq(1, zenith.bands, 1),
              h = seq(0, max(scan$h), 0.01))
  frame$shots <- NA
  frame$returns_below <- NA
  frame <- merge(bands, frame, by = "rings")

  #Set the extraction

  if(parallel == FALSE) {

    print(paste("", "Estimating the number of returns and shots", sep = ""))  #Progress bar
    pb <- txtProgressBar(min = 1, max = nrow(frame), style = 3)

    results <- foreach(i = 1:nrow(frame), .inorder = FALSE, .combine= rbind, .packages = c("data.table")) %do% {

      setTxtProgressBar(pb, i)

      frame$shots[i] <- nrow(scanner[between(zenith, frame$min.zenith[i], frame$max.zenith[i])])
      frame$returns_below[i] <- nrow(scan[between(zenith, frame$min.zenith[i], frame$max.zenith[i]) & Z <= frame$h[i]])

      return(frame[i])

    }
  }

  if(parallel == TRUE) {

    print("Estimating the number of returns and shots using parallel processig")
    pb <- txtProgressBar(min = 1, max = nrow(frame), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress=progress)

    cl <- makeCluster(cores, outfile="")
    registerDoSNOW(cl)

    results <- foreach(i = 1:nrow(frame), .inorder = FALSE, .combine= rbind, .packages = c("data.table"), .options.snow = opts) %dopar% {

      setTxtProgressBar(pb, i)

      frame$shots[i] <- nrow(scanner[between(zenith, frame$min.zenith[i], frame$max.zenith[i])])
      frame$returns_below[i] <- nrow(scan[between(zenith, frame$min.zenith[i], frame$max.zenith[i]) & Z <= frame$h[i]])

      return(frame[i])
    }

    close(pb)
    stopCluster(cl)
  }

  results[, Pgap := (1- (returns_below/shots)), by = seq_len(nrow(frame))]
}

