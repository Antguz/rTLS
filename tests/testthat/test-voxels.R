context("voxels")

test_that("Test whether the voxel and object function works", {

  data("pc_tree")

  pc <- pc_tree

  to_test <- voxels(pc_tree, edge_length = c(5, 5, 5))

  expect_equal(length(to_test), 3, info = "Length of the object")
  expect_equal(nrow(to_test$cloud), nrow(pc), info = "Identity of the cloud")
  expect_equal(length(to_test$parameter), 3, info = "Parameter of voxels")
  expect_equal(ncol(to_test$voxels), 4, info = "Columns of voxels")
  expect_equal(nrow(to_test$voxels), 6, info = "n of voxels")
  expect_equal(min(to_test$voxels$X), (min(pc$X) + 2.5), info = "X cordinate of voxels")
  expect_equal(min(to_test$voxels$Y), (min(pc$Y) + 2.5), info = "Y cordinate of voxels")
  expect_equal(min(to_test$voxels$Z), (min(pc$Z) + 2.5), info = "Z cordinate of voxels")
  expect_equal(sum(to_test$voxels$N), nrow(pc), info = "Total point in voxels")
})


test_that("Test whether the voxel function works", {

  data("pc_tree")

  pc <- pc_tree

  to_test <- voxels(pc_tree, edge_length = c(5, 5, 5), obj.voxels = FALSE)

  expect_equal(ncol(to_test), 4, info = "Columns of voxels")
  expect_equal(nrow(to_test), 6, info = "n of voxels")
  expect_equal(min(to_test$X), (min(pc$X) + 2.5), info = "X cordinate of voxels")
  expect_equal(min(to_test$Y), (min(pc$Y) + 2.5), info = "Y cordinate of voxels")
  expect_equal(min(to_test$Z), (min(pc$Z) + 2.5), info = "Z cordinate of voxels")
  expect_equal(sum(to_test$N), nrow(pc), info = "Total point in voxels")
})
