context("circleRANSAC")

test_that("Test whether the circleRANSAC works", {

  data("pc_tree")

  sub <- pc_tree[between(Z, 1.25, 1.35),]

  to_test <- circleRANSAC(sub, fpoints = 0.5, pconf = 0.99, poutlier = c(0.2, 0.2), max_iterations = 10000)

  expect_equal(length(to_test), 4, info = "Length of the object")
  expect_equal(nrow(to_test), 1, info = "N number of centers")
  expect_equal(round(to_test$X, 2), 9.25, info = "X of circle")
  expect_equal(round(to_test$Y, 2), -1.02, info = "Y of circle")
  expect_equal(round(to_test$radius, 2), 0.1, info = "radius")
  expect_equal(round(to_test$RMSE, 2), 0, info = "RMSE")

})
