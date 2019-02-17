a <- voxels_counting(pc_tree, voxel.range = NULL, random = FALSE,  bootstrap = FALSE, R = 100, parallel = TRUE, cores = 4)
b <- voxels_counting(pc_tree, voxel.range = NULL, random = TRUE,  bootstrap = FALSE, R = 100, parallel = TRUE, cores = 4)

a <- a[4:11,]
b <- b[4:11,]

ra <- lm(log10(a$N_voxels) ~ log10(a$Voxel.size))
rb <- lm(log10(b$N_voxels) ~ log10(b$Voxel.size))

as <- ra$coefficients[2]
bs <- rb$coefficients[2]

coef <- 1 - as/bs


plot(log10(a$N_voxels) ~ log10(a$Voxel.size))
abline(ra)

plot(log10(b$N_voxels) ~ log10(b$Voxel.size))
abline(rb)

total.size <- 6.34524972^3

total.size * coef
