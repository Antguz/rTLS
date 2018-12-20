#' @import dplyr
#'
#' @title
#'
#' @description
#'
#' @param x
#' @param cloud
#' @param k
#'
#'@export
knn_dimensionality <- function(x, cloud, k) {
  n_knn <- knn(data = as.matrix(cloud[,1:3]) , query = as.matrix(x[,1:3]), k = 10+1, eps = 0, searchtype = 1L, radius = 0)
  kpoints <- as.vector(n_knn$nn.idx)[2:(k+1)]
  distance <- as.vector(n_knn$nn.dists)[2:(k+1)]
  cube <- as.data.frame(cloud[kpoints, 1:3])

  space <- as.matrix(cube[, 1:3])

  if(length(space[,1]) >= 3) {
    pca <- prcomp(space[,1:3], center = TRUE, scale = FALSE, retx = FALSE)
    eigval <- pca$sdev^2

    frame <- data.frame(linearity = (eigval[1]-eigval[2])/eigval[1],
                        planarity = (eigval[2]-eigval[3])/eigval[1],
                        scattering = eigval[3]/eigval[1],
                        omnivariance = (eigval[1]*eigval[2]*eigval[3])^(1/3),
                        anisotropy = (eigval[1]-eigval[3])/eigval[1],
                        eigenentropy = -((eigval[1] * log(eigval[1])) + (eigval[2] * log(eigval[2])) + (eigval[3] * log(eigval[3]))),
                        sum_eigen = sum(eigval),
                        sur_var = min(eigval)/sum(eigval),
                        eigen_ratio = eigval[2]/eigval[2],
                        points = length(space[,1]))


  } else if(length(space[,1]) < 3) {
    frame <- data.frame(linearity = NA,
                        planarity = NA,
                        scattering = NA,
                        omnivariance = NA,
                        anisotropy = NA,
                        eigenentropy = NA,
                        sum_eigen = NA,
                        sur_var = NA,
                        eigen_ratio = NA,
                        points = length(space[,1]))
  }

  frame
}
