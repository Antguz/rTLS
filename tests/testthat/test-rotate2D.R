### Rotate2D

test_that("Test whether the rotation 2D works", {

  cartesian <- data.table(X = c(0, 0, 0, 0, 0, 0, 0, 0),
                          Y = c(0, 0, 1, 1, 1, -1, -1, -1),
                          Z = c(0, 10, 0, 5, 10, 0, 5, 10))

  to_test_0 <- round(rotate2D(cartesian[, 1:2], angle = 0), 2)

  to_test_45 <- round(rotate2D(cartesian[, 1:2], angle = 45), 2)

  rotation_45 <- data.table(X = c(0.00, 0.00, -0.71, -0.71, -0.71,  0.71,  0.71,  0.71),
                         Y = c(0.00, 0.00,  0.71,  0.71,  0.71, -0.71, -0.71, -0.71))

  expect_equal(to_test_0, cartesian[, 1:2], info = "Rotation of 0 degress")
  expect_equal(to_test_45, rotation_45, info = "Rotation of 45 degress")

})
