context("ray_AABB")

test_that("Test whether the ray_AABB works", {

  orig <- data.table(X = c(1, 1, 1), Y = c(0.75, 1, 1.25), Z = c(1, 1, 1))
  dir <- data.table(X = c(1, 1, 1), Y = c(0.75, 1, 1.25), Z = c(2.5, 4, 1.5))
  voxel_cor <- matrix(c(1, 1, 2.5), ncol = 3)
  edge_length <- c(1, 1, 1)
  voxel_min <- c(voxel_cor[1, 1] - edge_length[1]/2,
                 voxel_cor[1, 2] - edge_length[2]/2,
                 voxel_cor[1, 3] - edge_length[3]/2)
  voxel_max <- c(voxel_cor[1, 1] + edge_length[1]/2,
                 voxel_cor[1, 2] + edge_length[2]/2,
                 voxel_cor[1, 3] + edge_length[3]/2)

  test_1 <- ray_AABB(orig[1,], dir[1,], voxel_min, voxel_max)
  test_2 <- ray_AABB(orig[2,], dir[2,], voxel_min, voxel_max)
  test_0 <- ray_AABB(orig[3,], dir[3,], voxel_min, voxel_max)

  expect_equal(test_1, 1, info = "Intercepte")
  expect_equal(test_2, 2, info = "Intercepted with exit")
  expect_equal(test_0, 0, info = "Non-intercepted")
})
