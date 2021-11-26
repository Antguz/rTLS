#' rTLS: Tools to Process Point Clouds Derived From Terrestrial Laser Scanning
#'
#' rTLS is a package that compiles a set of tools to process and calculate
#' metrics on point clouds derived from terrestrial
#' LiDAR (Light Detection and Ranging). Its creation is based on key
#' aspects of the TLS application in forestry. Currently, the main
#' routines are based on filtering, neighboring features of points,
#' voxelization, optimal sphere or voxel size, and the creation of
#' artificial stands. rTLS is written using data.table and C++ language and in most of the
#' functions it is posible to use parallel processing to speed-up the routines.

#' @import data.table
"_PACKAGE"

utils::globalVariables(c(".", "Target_count", "Voxel.size",
                         "X", "Y", "Z", "azimuth", "cumsum_returns",
                         "i", "pulses", "returns", "w", "zenith", "Edge.X",
                         "N", "N_voxels", "query", "k_index"))
