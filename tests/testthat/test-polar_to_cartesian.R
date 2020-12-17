context("polar_to_cartesian")

test_that("Whether coordinate transformation works", {

  polar <- data.table(zenith = c(0, 0, 90, 11.3099, 5.7106, 90.0000, 11.3099, 5.7106),
                      azimuth = c(0, 0, 90, 90, 90, 270, 270, 270),
                      distance = c(0.0000, 10.0000, 1.0000, 5.0990, 10.0499, 1.0000, 5.0990, 10.0499))

  to_test <- round(polar_to_cartesian(polar), 0)

  cartesian <- data.table(X = c(0, 0, 0, 0, 0, 0, 0, 0),
                          Y = c(0, 0, 1, 1, 1, -1, -1, -1),
                          Z = c(0, 10, 0, 5, 10, 0, 5, 10))

  expect_equal(to_test, cartesian, info = "Transformation")
})
