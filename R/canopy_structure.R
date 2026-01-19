#' @title Canopy Structure
#'
#' @description Estimates the canopy structure from a discrete returns scan from different TLS.
#'
#' @param TLS.type A \code{character} describing is the TLS used. It most be one of \code{"single"} return, \code{"multiple"} return, or \code{"fixed.angle"} scanner.
#' @param scan If \code{TLS.type} is equal to \code{"single"} or \code{"fixed.angle"}, a \code{data.table} with three columns describing *XYZ* coordinates of the discrete return. If
#' \code{TLS.type} is equal to \code{"multiple"}, a \code{data.table} with four columns describing *XYZ* coordinates and the target count pulses. Currently, \code{"fixed.angle"} present errors, use with discretion.
#' @param zenith.range If \code{TLS.type} is equal to \code{"single"} or \code{"multiple"}, a \code{numeric} vector of length two describing the \code{min} and \code{max} range of the zenith angle to use.
#' Theoretically, the \code{max} range should be lower than 90 degrees.
#' @param zenith.rings If \code{TLS.type} is equal to \code{"single"} or \code{"multiple"}, a \code{numeric} vector of length one describing the number of zenith rings to use between \code{zenith.range}.
#' This is used to estimate the frequency of laser shots from the scanner and returns in \code{scan}. If \code{TLS.type = "fixed.angle"}, \code{zenith.rings = 1} be default.
#' @param azimuth.range A \code{numeric} vector of length two describing the range of the azimuth angle to use. Theoretically, it should be between 0 and 360 degrees.
#' @param vertical.resolution A \code{numeric} vector of length one describing the vertical resolution to extract the vertical profiles. Low values lead to more variable profiles.
#' The scale used needs to be in congruence with the scale of \code{scan}.
#' @param TLS.pulse.counts If \code{TLS.type} is equal to \code{"single"} or \code{"multiple"}, a \code{numeric} vector of length two describing the horizontal and vertical pulse counts of the scanner.
#' If \code{TLS.type} is equal to \code{"fixed.angle"}, a \code{numeric} vector of length one describing the horizontal pulse counts resolution.
#' Preferred parameter over \code{TLS.resolution} to estimate the number of pulses.
#' @param TLS.resolution If \code{TLS.pulse.counts = NULL}, the code use the angles resolution to estimate the pulse counts in a given \code{TLS.frame}. If \code{TLS.type} is equal to \code{"single"} or \code{"multiple"}, a \code{numeric} vector of length two describing the horizontal and vertical angle resolution of the scanner.
#' If \code{TLS.type} is equal to \code{"fixed.angle"}, a \code{numeric} vector of length one describing the horizontal angle resolution.
#' @param TLS.frame If \code{TLS.type} is equal to \code{"single"} or \code{"multiple"}, a \code{numeric} vector of length four describing the \code{min} and \code{max} of the zenith and azimuth angle of the scanner frame.
#' If \code{TLS.type = "fixed.angle"}, a \code{numeric} vector of length three describing the fixed zenith angle and the \code{min} and \code{max} of the azimuth angle of the scanner frame.
#' If \code{NULL}, it assumes that a complete hemisphere (\code{c(zenith.min = 0, zenith.max = 90, azimuth.min = 0, azimuth.max = 360)}), or a cone projection (\code{c(zenith = 57.5, azimuth.min = 0, azimuth.max = 360)}) depending on \code{TLS.type}.
#' @param TLS.angles A \code{numeric} vector of length three describing the roll (*X*), pitch (*Y*), and yaw (*Z*) angles of the scanner during the scan.
#' If \code{NULL}, it assumes that there is no need to to correction of angles.
#' This needs to be used if \code{TLS.type} is equal to \code{"single"} or \code{"multiple"}, since it assumes that \code{"fixed.angle"} scanner is previously balanced. \code{NULL} as default.
#' @param TLS.coordinates A \code{numeric} vector of length three describing the scanner coordinates within \code{scan}.
#' It assumes that the coordinates are \code{c(X = 0, Y = 0, Z = 0)} for default.
#' @param threads An \code{integer} specifying the number of threads to use. Experiment to see what works best for your data on your hardware.
#'
#' @details Since \code{scan} describes discrete returns measured by the TLS, \code{canopy_structre} first simulates the number of pulses emitted based on Danson et al. (2007). The simulated pulses are
#' created based on the TLS properties (\code{TLS.pulse.counts, TLS.resolution, TLS.frame}) assuming that the scanner is perfectly balance. Then these pulses are rotated (\code{\link{rotate3D}}) based on the \code{TLS.angles}
#' roll, pitch, and yaw, and move to \code{TLS.coordintates} to simulate the positioning of the scanner during the \code{scan}. Rotated simulated-pulses of interest and \code{scan} returns are then extracted based on the \code{zenith.range} and \code{azimuth.range} for a given number of \code{zenith.rings}, \code{azimuth.rings} and vertical profiles.
#' The probability of gap (Pgap) is then estimated using the frequency of pulses and returns. For \code{TLS.type = "multiple"}, the frequency of returns is estimated using the sum of 1/target count following Lovell et al. (2011).
#'
#' Using the Pgap estimated per each zenith ring and vertical profile, \code{canopy_structure} then estimates the accumulative L(z) profiles based on the closest
#' zenith ring to 57.5 (hinge region) and, if \code{TLS.type = "fixed.angle"}, the f(z) or commonly named PAVD based on the ratio of the
#' derivative of L(z) and height (z) following Jupp et al. 2009 (Equation 18). If \code{TLS.type} is equal to \code{"single"} or \code{"multiple"}, \code{canopy_structure} also
#' estimates the normalized average weighted L/LAI, and then PAVD based on the L (hinge angle) at the highest height (LAI) and the ratio between the derivative
#' of L/LAI (average weighted) and the derivative of z (Jupp et al. 2009; Equation 21).
#'
#' Jupp et al. 2009 excludes the zero zenith or fist ring to conduct the average weighted L/LAI estimations, \code{canopy_structre} does not excludes this sections since it depends on the regions of interest of the user.
#' Therefore, user should consider this difference since it may introduce more variability to profile estimations.
#'
#' @references
#' Danson F.M., Hetherington D., Morsdorf F., Koetz B., Allgower B. 2007. Forest canopy gap fraction from terrestrial laser scanning. IEEE Geosci. Remote Sensing Letters 4:157-160. doi: 10.1109/LGRS.2006.887064
#'
#' Lovell J.L., Jupp D.L.B., van Gorsel E., Jimenez-Berni J., Hopkinson C., Chasmer L. 2011. Foliage profiles from ground based waveform and discrete point LiDAR. In: SilviLaser 2011, Hobart, Australia, 16–20 October 2011.
#'
#' Jupp D.L.B., Culvenor D.S., Lovell J.L., Newnham G.J., Strahler A.H., Woodcock C.E. 2009. Estimating forest LAI profiles and structural parameters using a ground-based laser called “Echidna®”. Tree Physiology 29(2): 171-181. doi: 10.1093/treephys/tpn022
#'
#' @return For any \code{TLS.type}, it returns a \code{data.table} with the height profiles defined by \code{vertical.resolution}, the gap probability based on the \code{zenith.range} and \code{zenith.rings}, and
#' the accumulative L(z) profiles based on the closest zenith ring to 57.5 degrees (hinge angle). If \code{TLS.type} is equal to \code{"fixed.angle"}, it returns f(z) or commonly named PAVD based on
#' on the ratio of the derivative of L(z) and the derivative of height (z). If \code{TLS.type} is equal to \code{"single"} or \code{"multiple"}, it returns the normalized average weighting L/LAI, and PAVD: based
#' on the L (hinge angle) at the highest height and the ratio between the derivative of L/LAI average weighted and the derivative of z.
#'
#'
#' @author J. Antonio Guzmán Q.
#'
#' @importFrom stats reshape
#' @importFrom stats weighted.mean
#' @import data.table
#'
#' @examples
#'
#' \donttest{
#' data(TLS_scan)
#' #Using a multiple return file
#' #Select the four columns required
#' TLS_scan <- TLS_scan[, 1:4]
#'
#' #This will take a while#
#' canopy_structure(TLS.type = "multiple",
#'                  scan = TLS_scan,
#'                  zenith.range = c(50, 70),
#'                  zenith.rings = 4,
#'                  azimuth.range = c(0, 360),
#'                  vertical.resolution = 0.25,
#'                  TLS.pulse.counts = c(2082, 580),
#'                  TLS.frame = c(30, 130.024, 0, 359.90),
#'                  TLS.angles =  c(1.026, 0.760, -110.019))
#'
#' #Using a single return file
#'
#' data(TLS_scan)
#' #Subset to first return observations
#' TLS_scan <- TLS_scan[Target_index == 1, 1:3]
#'
#' #This will take a while#
#' canopy_structure(TLS.type = "single",
#'                  scan = TLS_scan,
#'                  zenith.range = c(50, 70),
#'                  zenith.rings = 4,
#'                  azimuth.range = c(0, 360),
#'                  vertical.resolution = 0.25,
#'                  TLS.pulse.counts = c(2082, 580),
#'                  TLS.frame = c(30, 130.024, 0, 359.90),
#'                  TLS.angles =  c(1.026, 0.760, -110.019))
#' }
#'
#' @export
canopy_structure <- function(TLS.type, scan, zenith.range, zenith.rings, azimuth.range, vertical.resolution, TLS.pulse.counts, TLS.resolution = NULL, TLS.coordinates = c(0, 0, 0), TLS.frame = NULL, TLS.angles = NULL, threads = 1) {

  if(TLS.type == "multiple") {
    colnames(scan)[1:4] <- c("X", "Y", "Z", "Target_count")
  } else if(TLS.type == "single" | TLS.type == "fixed.angle") {
    colnames(scan)[1:3] <- c("X", "Y", "Z")
  }

  ###Validate assumptions-------------------------------------------------------------------------------------------------------
  if(is.null(TLS.pulse.counts) == TRUE) {
    if(TLS.type == "single" | TLS.type == "multiple") { ####TLS resolution
      if(length(TLS.resolution) != 2 & is.numeric(TLS.resolution) != TRUE) {
        stop("The TLS.resolution needs to be a numeric vector of length two representing the horizontal and vertical resolution of the scanner")
      }
    } else if(TLS.type == "fixed.angle") {
      if(length(TLS.frame) != 1 & is.numeric(TLS.resolution) != TRUE) {
        stop("The TLS.resolution needs to be a numeric vector of length one representing the horizontal resolution of the scanner")
      }
    }
  }

  if(TLS.type == "single" | TLS.type == "multiple") { ####TLS frame
    if(is.null(TLS.frame) == TRUE) {
      TLS.frame <- c(zenith.min = 0, zenith.max = 90, azimuth.min = 0, azimuth.max = 360)
    } else if(length(TLS.frame) != 4 & is.numeric(TLS.frame) != TRUE) {
      stop("The TLS.frame needs to be a numeric vector of length four representing the min and max of the zenith and azimuth TLS scan")
    }
  } else if(TLS.type == "fixed.angle") {
    if(is.null(TLS.frame) == TRUE) {
      TLS.frame <- c(zenith = 57.5, azimuth.min = 0, azimuth.max = 360)
    } else if(length(TLS.frame) != 3 & is.numeric(TLS.frame) != TRUE) {
      stop("The TLS.frame needs to be a numeric vector of length three representing fixed zenith angle and the the min and max of the azimuth of the scanner")
    }
  }

  #####Move the scan returns----------------------------------------------------------------------------------------------------------------------------

  if(TLS.type == "multiple" | TLS.type == "single") {

    ####Estimating the value of the returns
    if(TLS.type == "multiple") {
      scan[, w := round(1/Target_count, 3),]
    } else if(TLS.type == "single" | TLS.type == "fixed.angle") {
      scan$w <- 1
    }

    if(is.null(TLS.angles) == TRUE) {
      scan_polar <- scan
    } else {
      scan_polar <- cbind(rotate3D(scan, roll = TLS.angles[1], pitch = TLS.angles[2], yaw = TLS.angles[3], threads), w = scan$w) ###Rotate scans
    }

    scan_polar <- cbind(cartesian_to_polar(scan_polar[, 1:3], TLS.coordinates, threads)[,1:2], scan_polar[, 3], w = scan_polar$w) ###Convert to polar
    scan_polar <- scan_polar[between(zenith, zenith.range[1], zenith.range[2]) & between(azimuth, azimuth.range[1], azimuth.range[2]),] ###Cut to zenith and azimuth angles
  }

  #####Simulate scanner pulses----------------------------------------------------------------------------------------------------------------------------

  if(TLS.type == "multiple" | TLS.type == "single") {  ##Estimate the number per single and multiple

    ###Simulate the scanning pulses
    if(is.null(TLS.pulse.counts) == FALSE) {
      scanner <- CJ(zenith = seq(TLS.frame[1], TLS.frame[2], length.out = TLS.pulse.counts[1]),
                    azimuth = seq(TLS.frame[3], TLS.frame[4], length.out = TLS.pulse.counts[2]))
    } else {
      scanner <- CJ(zenith = seq(TLS.frame[1], TLS.frame[2], TLS.resolution[1]),
                    azimuth = seq(TLS.frame[3], TLS.frame[4], TLS.resolution[2]))
    }

    scanner[, azimuth := ((azimuth*pi/360) %% pi)*360/pi, by = seq_along(1:nrow(scanner))] ###Azimuth between 0 and 360

    scanner$distance <- 1
    scanner <- polar_to_cartesian(scanner, threads)  #Convert to cartesian

    if(is.null(TLS.angles) != TRUE) {
      scanner <- rotate3D(scanner, roll = TLS.angles[1], pitch = TLS.angles[2], yaw = TLS.angles[3], threads) #Apply correction of angles
    }

    scanner <- cartesian_to_polar(scanner, TLS.coordinates, threads) #Convert to polar
    scanner <- scanner[between(zenith, zenith.range[1], zenith.range[2]) & between(azimuth, azimuth.range[1], azimuth.range[2]), 1:2] #Cut to zenith and azimuth angles
  }

  ####Set the table for results------------------------------------------------------------------------------------------------------------------

  if(TLS.type == "multiple" | TLS.type == "single") {

    ###Create the deviation of bands for profiles
    sd_zenith_bands <- ((zenith.range[2]-zenith.range[1])/zenith.rings)/2

    ###Create profiles
    zenith_bands <- seq(zenith.range[1]+sd_zenith_bands, zenith.range[2]-sd_zenith_bands, length.out = zenith.rings) ##Zenith bands
    height <- seq(0, ceiling(max(scan_polar[, 3])), vertical.resolution) ###Height vertical distribution

    ###Create the ranges for angles to cut
    cut_zenith <- c(zenith_bands[1]-sd_zenith_bands, zenith_bands + sd_zenith_bands)

    ###Create frame

    frame <- CJ(zenith_bands = as.factor(zenith_bands), height = as.factor(height[-1]))

  }

  ###Cut values to given categories---------------------------------------------------------------------------------------------------------------

  if(TLS.type == "multiple" | TLS.type == "single") {

    ###Cut scan
    scan_polar[, c("zenith_bands", "height") := list(cut(zenith, cut_zenith, labels = zenith_bands, include.lowest = TRUE),
                                                     cut(Z, height, labels = height[2:length(height)], include.lowest = TRUE))] ###Adding categories
    scan_polar <- scan_polar[, .(returns = sum(w)), by = c("zenith_bands", "height")] ####Sumarize values

    ###Cut scanner
    scanner[, c("zenith_bands") := list(cut(zenith, cut_zenith, zenith_bands, include.lowest = TRUE))]
    scanner <- scanner[, .(pulses = .N), by = c("zenith_bands")]
  }

  ######Estimates the gap fraction probability---------------------------------------------------------------------------------------

  if(TLS.type == "multiple" | TLS.type == "single") {

    Pgap <- merge(frame, scanner, by = c("zenith_bands"), all.x = TRUE, all.y= TRUE)
    Pgap <- merge(Pgap, scan_polar, by = c("zenith_bands", "height"), all.x = TRUE, all.y= TRUE)

    Pgap$returns[is.na(Pgap$returns)] <- 0
    colnames(Pgap)[1:2] <- c("zenith", "height")

    Pgap[, c("zenith", "height") := list(as.numeric(as.character(zenith)),
                                         as.numeric(as.character(height)))]

    Pgap <- Pgap[order(zenith, height)]

    Pgap[, cumsum_returns := cumsum(returns), by = c("zenith")]

    Pgap[, Pgap := (1- (cumsum_returns/pulses)), by = seq_len(nrow(Pgap))]

  }

  #####Estimation of the canopy structure metrics-------------------------------------------------------------------------------------

  if(TLS.type == "multiple" | TLS.type == "single") {

    Pgap[, L_LAI := log(Pgap)/log(min(Pgap)), by = zenith] #Normalize L/LAI

    zr <- zenith.rings
    Pgap[, zenith_idx := frank(zenith, ties.method = "first"), by = height]
    Pgap[, L_LAI_W := weighted.mean(L_LAI, w = zenith_idx, na.rm = TRUE), by = height]
    Pgap[, zenith_idx := NULL]

    final <- reshape(Pgap[, c("zenith" ,"height", "Pgap")],
                       v.names = "Pgap",
                       idvar = "height",
                       timevar= "zenith",
                       direction= "wide")

    colnames(final) <- c("height", paste("", "Pgap(", zenith_bands, ")", sep = ""))

    col_hinge <- which(abs(zenith_bands - 57.5) == min(abs(zenith_bands - 57.5)))
    subset_Pgap <- subset(Pgap, zenith == zenith_bands[col_hinge])

    final[, L := -1.1 * log(subset_Pgap$Pgap)] ###Estimates the L close to hinge
    final[, L_LAI_W := subset_Pgap$L_LAI_W]

    max_LAI <- as.numeric(final[which.max(height), L])

    final$PAVD <- NA

    for(i in 1:nrow(final)) {  ###Estimates PAVD
      ld <- final$L_LAI_W[i+1]-final$L_LAI_W[i]
      final$PAVD[i] <- max_LAI*(ld/vertical.resolution)
    }
  }

  # Change names
  final <- data.table::setnames(final,
                                old = c("L", "L_LAI_W"),
                                new = c("L (hinge)", "L/LAI (weighted mean)"))

  return(final)
}
