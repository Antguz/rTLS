context("filter")

test_that("Whether filter SOR works", {

  data("pc_tree")

  pc <- pc_tree

  to_SOR <- filter(pc, method = "SOR", k = 20, nSigma = 1, checks = 20)

  expect_equal(nrow(to_SOR), 65946, info = "Number of points")
  expect_equal(ncol(to_SOR), 3, info = "Number of columns")

})

test_that("Whether filter min_neighbors works", {

  point_cloud <- data.table(X = c(0, 0, 0, 0, 0, -1, 1),
                            Y = c(0, 0, 0, -1, 1, 0, 0),
                            Z = c(-1, 0, 1, 0, 0, 0, 0))

  to_min <- filter(point_cloud, method = "min_neighbors", radius = 2, min_neighbours = 2)

  expect_equal(nrow(to_min), 1, info = "Number of points")
  expect_equal(ncol(to_min), 3, info = "Number of columns")
  expect_equal(as.numeric(to_min[1,]), c(0, 0, 0), info = "Values")

})

test_that("Whether filter voxel_center works", {

  point_cloud <- data.table(X = c(0, 0, 0, 0, 0, -1, 1),
                            Y = c(0, 0, 0, -1, 1, 0, 0),
                            Z = c(-1, 0, 1, 0, 0, 0, 0))

  to_voxel <- filter(point_cloud, method = "voxel_center", edge_length = 3)

  expect_equal(nrow(to_voxel), 1, info = "Number of points")
  expect_equal(ncol(to_voxel), 3, info = "Number of columns")
  expect_equal(as.numeric(to_voxel[1,]), c(0, 1, 0), info = "Values")

})
