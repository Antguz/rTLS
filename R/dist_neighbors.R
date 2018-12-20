dist_neighbors <- function(x, cloud, radius) {
  cube <- cloud %>% filter(cloud[,1] <= (x[,1] + radius), cloud[,1] >= (x[,1] - radius),
                           cloud[,2] <= (x[,2] + radius), cloud[,2] >= (x[,2] - radius),
                           cloud[,3] <= (x[,3] + radius), cloud[,3] >= (x[,3] - radius))
  
  cube <- cube[,1:3]
  cube$distance <- as.vector(dist.xyz(x[,1:3], cube[,1:3]))
  neig <- cube$distance <= radius & cube$distance > 0
  as.matrix(cube[neig, 1:4])
}