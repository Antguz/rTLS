a <- voxels_counting(pc_tree, voxel.range = NULL, random = FALSE,  bootstrap = FALSE, parallel = TRUE, cores = 4)
b <- voxels_counting(pc_tree, voxel.range = NULL, random = TRUE,  bootstrap = FALSE, parallel = TRUE, cores = 4)

a
b

a <- a[3:11,]
b <- b[3:11,]

ra <- lm(log10(a$N_voxels) ~ log10((a$Voxel.size)^3))
rb <- lm(log10(b$N_voxels) ~ log10((b$Voxel.size)^3))

plot(log10(a$N_voxels) ~ log10((a$Voxel.size)^3))
abline(ra)

plot(log10(b$N_voxels) ~ log10((b$Voxel.size)^3))
abline(rb)

difference <- b$N_voxels-a$N_voxels
differenceH <- b$Negentropy - a$Negentropy

plot(difference ~ a$Voxel.size)

as <- ra$coefficients[2]
bs <- rb$coefficients[2]

as <- -ra$coefficients[1] / ra$coefficients[2]
bs <- -rb$coefficients[1] / rb$coefficients[2]



((bs-as))*0.671

(bs/as - 1) * (6.34524972^3)

coef <- 1 - (as-bs)

bs-as

as/bs*(6.34524972^3)

(bs/as) - 1

total.size <- 6.34524972^3

(bs-as)*0.75

5.109088 * 0.671

