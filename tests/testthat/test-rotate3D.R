### Rotate3D

test_that("Test whether the rotation 3D works", {

  cartesian <- data.table(X = c(0, 0, 0, 0, 0, 0, 0, 0),
                          Y = c(0, 0, 1, 1, 1, -1, -1, -1),
                          Z = c(0, 10, 0, 5, 10, 0, 5, 10))

  to_test_0 <- round(rotate3D(cartesian, roll = 0, pitch = 0, yaw = 0), 2)

  to_test_45 <- round(rotate3D(cartesian, roll = 45, pitch = 45, yaw = 45), 2)

  rotation_45 <- data.table(X = c(0.00,  8.54, -0.15,  4.12,  8.39,  0.15,  4.41,  8.68),
                            Y = c(0.00, -1.46,  0.85,  0.12, -0.61, -0.85, -1.59, -2.32),
                            Z = c(0.0,  5.0,  0.5,  3.0,  5.5, -0.5,  2.0,  4.5))

  expect_equal(to_test_0, cartesian, info = "3D Rotation of 0 degress")
  expect_equal(to_test_45, rotation_45, info = "3D Rotation of 45 degress")

})
