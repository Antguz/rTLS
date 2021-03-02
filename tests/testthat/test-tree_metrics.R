context("tree_metrics")

test_that("Test whether the tree_metrics works", {

  data("pc_tree")

  to_test <- tree_metrics(pc_tree)

  expect_equal(round(to_test$Height, 4), 6.0365, info = "Height")
  expect_equal(round(to_test$Crown_area, 4), 28.5489, info = "Crown_area")
  expect_equal(round(to_test$DBH, 4), 0.2002, info = "DBH")

})
