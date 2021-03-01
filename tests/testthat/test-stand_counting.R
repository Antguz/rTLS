context("stand_counting")

test_that("Test whether the stand_counting function works", {

  data("pc_tree")

  pc <- pc_tree

  to_test <- stand_counting(pc, xy.res = c(4, 4), min_size = 2, progress = FALSE)

  expect_equal(length(to_test), 14, info = "Length of the object")
  expect_equal(nrow(to_test), 40, info = "N of voxel sizes")
  expect_equal(length(unique(to_test$X)), 2, info = "N of voxels on X")
  expect_equal(length(unique(to_test$Y)), 2, info = "N of voxels on Y")
})
