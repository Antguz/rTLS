fractal_dopar <- function(data, sizes) {

  results <- foreach(i = 1:length(sizes), .inorder = FALSE, .combine= 'rbind', .packages = c("dplyr", "boot"), .export=c("voxels", "shannon", "shannon_boot")) %dopar% {
    vox <- voxels(data[,1:3], voxel.size = sizes[i], precision = 3)
    vox <- subset(vox, n >= 1)
    vox$eq <- 1
    Count <- length(vox$n) #Numero de voxels
    Density_mean <- mean(vox$n/(sizes[i]^3))
    Density_var <- var(vox$n/(sizes[i]^3))
    H <- shannon(vox$n) #H index
    h_boot <- boot(vox$n, shannon_boot, R= 100)$t
    H_boot <- mean(h_boot) #H index with boot
    Hsd_boot <- sd(h_boot)
    Hmax <- shannon(vox$eq) #H max
    Equitavility <- H/Hmax #Equitavility
    Equitavility_boot <- H_boot/Hmax #Equitavility based on boot
    I <- Hmax - H #Negentropy
    I_boot <- Hmax - H_boot #Negentropy based on boot
    c(sizes[i], Count, Density_mean, Density_var, H, H_boot, Hsd_boot, Hmax, Equitavility, Equitavility_boot, I, I_boot)
  }

  names <- c("Size", "Count", "Density_mean", "Density_var", "H", "H_boot", "Hsd_boot", "Hmax", "Equitavility", "Equitavility_boot", "I", "I_boot")
  results <- as.data.frame(results)
  colnames(results) <- names
  rownames(results) <-c()
  results
}

shannon <- function(n_points) {
  p.i <- n_points/sum(n_points)
  H <- (-1) * sum(p.i * log(p.i))
  return(H)
}

shannon_boot <- function(n_points, i) {
  n_boot <- n_points[i]
  p.i <- n_boot/sum(n_boot)
  H <- (-1) * sum(p.i * log(p.i))
  return(H)
}
