### Min_distance

test_that("Test whether the minimum distance calculation works", {

  cartesian <- data.table(X = c(0, 0, 0, 0, 0, 0, 0, 0),
                          Y = c(0, 0, 1, 1, 1, -1, -1, -1),
                          Z = c(0, 10, 0, 5, 10, 0, 5, 10))

  to_test <- min_distance(cartesian)

  expect_equal(to_test, 1, info = "Min distance")
})
