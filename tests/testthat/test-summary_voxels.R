context("summary_voxels")

test_that("Test whether the summary applied on voxels works", {

  data("pc_tree")

  pc <- pc_tree

  vox <- voxels(pc_tree, edge_length = c(5, 5, 5))
  to_test <- summary_voxels(vox, bootstrap = TRUE, R = 1000)

  expect_equal(as.numeric(to_test[1, 1]), 5, info = "Edge X")
  expect_equal(as.numeric(to_test[1, 2]), 5, info = "Edge Y")
  expect_equal(as.numeric(to_test[1, 3]), 5, info = "Edge Z")
  expect_equal(as.numeric(to_test[1, 4]), 6, info = "n of voxels")
  expect_equal(as.numeric(to_test[1, 5]), 750, info = "Volume")
  expect_equal(as.numeric(to_test[1, 6]), 100, info = "Surface")
  expect_equal(round(as.numeric(to_test[1, 7]), 2), 505.65, info = "Density")
  expect_equal(round(as.numeric(to_test[1, 8]), 2), 894.45, info = "Density_sd")
  expect_equal(round(as.numeric(to_test[1, 9]), 2), 0.84, info = "H")
  expect_equal(round(as.numeric(to_test[1, 10]), 2), 1.79, info = "Hmax")
  expect_equal(round(as.numeric(to_test[1, 11]), 2), 0.47, info = "Equitavility")
  expect_equal(round(as.numeric(to_test[1, 12]), 2), 0.95, info = "Negentropy")
  expect_equal(ncol(to_test), 16, info = "ncol")
})


test_that("Test whether the summary applied on pc works", {

  data("pc_tree")

  pc <- pc_tree

  vox <- voxels(pc_tree, edge_length = c(5, 5, 5), obj.voxels = FALSE)
  to_test <- try(summary_voxels(vox, edge_length = c(5, 5, 5), bootstrap = TRUE, R = 1000), silent = FALSE)

  expect_equal(as.numeric(to_test[1, 1]), 5, info = "Edge X")
  expect_equal(as.numeric(to_test[1, 2]), 5, info = "Edge Y")
  expect_equal(as.numeric(to_test[1, 3]), 5, info = "Edge Z")
  expect_equal(as.numeric(to_test[1, 4]), 6, info = "n of voxels")
  expect_equal(as.numeric(to_test[1, 5]), 750, info = "Volume")
  expect_equal(as.numeric(to_test[1, 6]), 100, info = "Surface")
  expect_equal(round(as.numeric(to_test[1, 7]), 2), 505.65, info = "Density")
  expect_equal(round(as.numeric(to_test[1, 8]), 2), 894.45, info = "Density_sd")
  expect_equal(round(as.numeric(to_test[1, 9]), 2), 0.84, info = "H")
  expect_equal(round(as.numeric(to_test[1, 10]), 2), 1.79, info = "Hmax")
  expect_equal(round(as.numeric(to_test[1, 11]), 2), 0.47, info = "Equitavility")
  expect_equal(round(as.numeric(to_test[1, 12]), 2), 0.95, info = "Negentropy")
  expect_equal(ncol(to_test), 16, info = "ncol")
})
