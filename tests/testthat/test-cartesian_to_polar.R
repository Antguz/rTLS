context("cartesian_to_polar")

test_that("Whether coordinate transformation works", {

  pc <- data.table(X = c(0, 0, 0, 0, 0, 0, 0, 0),
                   Y = c(0, 0, 1, 1, 1, -1, -1, -1),
                   Z = c(0, 10, 0, 5, 10, 0, 5, 10))

  anchor <- c(0, 0, 5)

  to_test <- round(cartesian_to_polar(pc, anchor), 4)

  correct_values <- data.table(zenith = c(180.0000, 0.0000, 168.6901, 90.0000, 11.3099, 168.6901, 90.0000, 11.3099),
                               azimuth = c(0, 0, 90, 90, 90, 270, 270, 270),
                               distance = c(5.000, 5.000, 5.099, 1.000, 5.099, 5.099, 1.000, 5.099))

  expect_equal(to_test, correct_values, info = "Transformation")
})
