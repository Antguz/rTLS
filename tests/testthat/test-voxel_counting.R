### Voxels_counting

test_that("Test whether the voxels_counting function works", {

  data("pc_tree")

  pc <- pc_tree

  to_test <- voxels_counting(pc, min_size = 0.5, progress = FALSE)

  expect_equal(length(to_test), 12, info = "Length of the object")
  expect_equal(nrow(to_test), 10, info = "N of voxel sizes")
})


test_that("Test whether the voxels_counting function works with predifined sizes", {

  data("pc_tree")

  pc <- pc_tree

  sizes <- c(1, 2, 3, 4, 5, 6)

  to_test <- voxels_counting(pc, edge_sizes = sizes, progress = FALSE)

  expect_equal(length(to_test), 12, info = "Length of the object")
  expect_equal(nrow(to_test), 6, info = "N of voxel sizes")
  expect_equal(to_test$Edge.X, 1:6, info = "Voxel size match")
})
