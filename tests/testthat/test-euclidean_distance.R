context("euclidean_distance")

test_that("Test whether the euclidean distance estimation works", {

  cartesian <- data.table(X = c(0, 0, 0, 0, 0, 0, 0, 0),
                          Y = c(0, 0, 1, 1, 1, -1, -1, -1),
                          Z = c(0, 10, 0, 5, 10, 0, 5, 10))

  to_test <- round(euclidean_distance(c(0, 0, 5), cartesian), 2)

  distances <- c(5.0, 5.0, 5.1, 1.0, 5.1, 5.1, 1.0, 5.1)

  expect_equal(to_test, distances, info = "Euclidean distance")
})
