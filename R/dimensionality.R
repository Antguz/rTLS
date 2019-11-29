#' @title Dimensionality of the Neighboring Points.
#'
#' @description Calculate the dimensionality of a cloud of neighboring points. It estimates nine parameters based on Wang et al. 2017.
#'
#' @param space A \code{data.table} with *XYZ* coordinates of the neighboring points in the first three columns.
#'
#' @details Since the extraction of the metrics are based on \code{\link{eigen}}, a \code{space} of neighboring points lower than three may return \code{NA} as results.
#'
#' @return A \code{data.table} with the estimated parameters.
#' @author J. Antonio Guzmán Q. and Ronny Hernandez
#' @references Wang, D., Hollaus, M., & Pfeifer, N. (2017). Feasibility of machine learning methods for separating wood and leaf points from Terrestrial Laser Scanning data. ISPRS Annals of Photogrammetry. Remote Sensing & Spatial Information Sciences, 4. <doi:10.5194/isprs-annals-IV-2-W4-157-2017>
#'
#' @seealso \code{\link{basic_metrics}}, \code{\link{distribution}}, \code{\link{cloud_metrics}}, \code{\link{neighborhood}}
#'
#' @importFrom stats na.exclude
#' @importFrom coop covar
#'
#' @examples
#' data("pc_tree")
#' neig <- neighborhood(pc_tree[50,], pc_tree, method = "sphere", radius = 0.2)
#' dimensionality(neig$neighborhood[, c(2:4)])
#'
#' @export
dimensionality <- function(space) {

  space <- na.exclude(space)

  if(nrow(space) >= 3) {
    eigval <- eigen(covar(space), only.values = TRUE)$values

    frame <- data.table(linearity = (eigval[1]-eigval[2])/eigval[1],
                        planarity = (eigval[2]-eigval[3])/eigval[1],
                        scattering = eigval[3]/eigval[1],
                        omnivariance = (eigval[1]*eigval[2]*eigval[3])^(1/3),
                        anisotropy = (eigval[1]-eigval[3])/eigval[1],
                        eigenentropy = -((eigval[1] * log(eigval[1])) + (eigval[2] * log(eigval[2])) + (eigval[3] * log(eigval[3]))),
                        sum_eigen = sum(eigval),
                        sur_var = min(eigval)/sum(eigval),
                        eigen_ratio_2D = eigval[2]/eigval[1])

  } else if(nrow(space) < 3) {
    frame <- data.table(linearity = as.numeric(NA),
                        planarity = as.numeric(NA),
                        scattering = as.numeric(NA),
                        omnivariance = as.numeric(NA),
                        anisotropy = as.numeric(NA),
                        eigenentropy = as.numeric(NA),
                        sum_eigen = as.numeric(NA),
                        sur_var = as.numeric(NA),
                        eigen_ratio_2D = as.numeric(NA))
  }
  return(frame)
}
