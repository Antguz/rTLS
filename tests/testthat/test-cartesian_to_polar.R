context("cartesian_to_polar")

test_that("Whether coordinate transformation works", {

  cartesian <- data.table(X = c(0, 0, 0, 0, 0, 0, 0, 0),
                   Y = c(0, 0, 1, 1, 1, -1, -1, -1),
                   Z = c(0, 10, 0, 5, 10, 0, 5, 10))

  anchor <- c(0, 0, 0)

  to_test <- round(cartesian_to_polar(cartesian, anchor), 4)

  polar <- data.table(zenith = c(NaN, 0, 90, 11.3099, 5.7106, 90.0000, 11.3099, 5.7106),
                               azimuth = c(0, 0, 90, 90, 90, 270, 270, 270),
                               distance = c(0.0000, 10.0000, 1.0000, 5.0990, 10.0499, 1.0000, 5.0990, 10.0499))

  expect_equal(to_test, polar, info = "Transformation")
})
