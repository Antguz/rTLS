knn_neighbors <- function(x, cloud, k) {
  n_knn <- knn(data = as.matrix(cloud[,1:3]) , query = as.matrix(x[,1:3]), k = 10+1, eps = 0, searchtype = 1L, radius = 0)
  kpoints <- as.vector(n_knn$nn.idx)[2:(k+1)]
  distance <- as.vector(n_knn$nn.dists)[2:(k+1)]
  cube <- as.data.frame(cloud[kpoints, 1:3])
  cube$distance <- distance
  as.matrix(cube)

}