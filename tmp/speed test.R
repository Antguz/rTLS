##########################################################################
######This is a speed test in order to choose the best function###########
##########################################################################

###With plyr
#---------------------------------------#
data(pc_tree)
cloud <- pc_tree[1:5000,]
cloud_b <- pc_tree
radius <- 0.5
parallel = FALSE

system.time(results_1 <- alply(cloud, .margins = 1, .fun = sphere_neighbors, cloud = cloud_b, radius = radius, .progress = "text", .parallel = parallel, .paropts = pack, .inform = FALSE))


###With purrrlyr
#---------------------------------------#
data(pc_tree)
pc_tree <- as_tibble(pc_tree)
cloud <- pc_tree[1:5000,]
cloud_b <- pc_tree
radius <- 0.5

system.time(results_2 <- cloud %>% by_row(..f = sphere_neighbors, cloud = cloud_b, radius = radius, .collate = "list", .to = "neighborhood"))


###With dplyr
#---------------------------------------#
data(pc_tree)
pc_tree <- as_tibble(pc_tree)
cloud <- pc_tree[1:5000,]
cloud_b <- pc_tree
radius <- 0.5

system.time(results_3 <- cloud %>% rowwise %>% do(neighborhood = sphere_neighbors(data_frame(.$X, .$Y, .$Z) , cloud = cloud_b, radius = radius)))

###With data.table
#---------------------------------------#
data(pc_tree)
pc_tree <- as.data.table(pc_tree)
cloud <- pc_tree[1:5000,]
cloud_b <-  pc_tree
radius <- 0.5

sphere_neighbors2 <- function(x, cloud, radius) {

  xcoor <- as.numeric(x[1,1])
  ycoor <- as.numeric(x[1,2])
  zcoor <- as.numeric(x[1,3])

  cube <- cloud[between(X, xcoor - radius, xcoor + radius) & between(Y, ycoor - radius, ycoor + radius) & between(Z, zcoor - radius, zcoor + radius),]

  cube <- cube[,1:3]

  cube <- cube[,distance := sqrt((xcoor - cube$X)^2 + (ycoor - cube$Y)^2 + (zcoor - cube$Z)^2)]

  space <- cube[cube$distance <= radius & cube$distance > 0,]
  space <- space[order(distance),]
  return(space)
}

system.time(results_4 <- cloud[, sphere_neighbors2(.SD, cloud_b, radius), by = seq_len(nrow(cloud))])


