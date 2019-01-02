fractal <- function(data, sizes) {
  xyz <- data[,1:3]
  frame <- data.frame(Size = sizes,
                      Count = NA,
                      Density_mean = NA,
                      Density_var = NA,
                      H = NA,
                      Hmax = NA,
                      Equitavility = NA,
                      I = NA)

  for(i in 1:length(sizes)) {
    vox <- voxels(xyz, voxel.size = sizes[i])
    frame[i, 2] <- length(vox$n) #Numero de voxels
    frame[i, 3] <- mean(vox$n/(sizes[i]^3))
    frame[i, 4] <- var(vox$n/(sizes[i]^3))
    points <- sum(vox$n)
    frame[i, 5] <- -sum((vox$n/points) * log(vox$n/points)) #H index
    frame[i, 6] <- log(length(vox$n)) #H max
    frame[i, 7] <- frame[i, 5]/frame[i, 6] #Equitavility
    frame[i, 8] <- frame[i, 6]-frame[i, 5] #Negentropy
  }
  frame
}

