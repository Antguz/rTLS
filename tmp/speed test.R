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

system.time(results_1 <- alply(cloud, .margins = 1, .fun = sphere_neighbors, cloud = cloud, radius = radius, .progress = "text", .parallel = parallel, .paropts = pack, .inform = FALSE))


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
