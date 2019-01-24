#' @import dplyr
#'
#' @title Dimensionality of the neighboring points.
#'
#' @description Calculate the dimensionality of a cloud of neighboring points. It estimate 10 parameters based on Wang et al. 2017.
#'
#' @param space A \code{matrix} or \code{data.frame} with xzy coordinate in the first three columns.
#'
#' @return A \code{data.frame} with the estimated parameters
#' @author J. Antonio Guzman Q. and Ronny Hernandez
#' @references Wang, D., Hollaus, M., & Pfeifer, N. (2017). Feasibility of machine learning methods for separating wood and leaf points from Terrestrial Laser Scanning data. ISPRS Annals of Photogrammetry, Remote Sensing & Spatial Information Sciences, 4.
#'
#' @examples
#' data("pc_tree")
#'
#' neig <- neighborhood(pc_tree[50,], pc_tree, method = "sphere", radius = 0.2)
#'
#' dimensionality(neig$neighborhood[, c(2:4)])
#'
#' @export
dimensionality <- function(space) {

  if(nrow(space) >= 3) {
    pca <- prcomp(space[,1:3], center = TRUE, scale = FALSE, retx = FALSE)
    eigval <- pca$sdev^2

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
    frame <- data.table(linearity = NA,
                        planarity = NA,
                        scattering = NA,
                        omnivariance = NA,
                        anisotropy = NA,
                        eigenentropy = NA,
                        sum_eigen = NA,
                        sur_var = NA,
                        eigen_ratio_2D = NA)
  }
  return(frame)
}
