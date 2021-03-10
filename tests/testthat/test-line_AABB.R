context("line_AABB")
test_that("Test whether the line_AABB works", {

  orig <- data.table(X = c(0, 0, 0, 0, 0),
                     Y = c(-0.45, -0.25, 0, 0.25, 0.45),
                     Z = c(-1, -0.25, 0, -1, -1))

  end <- data.table(X = c(0, 0, 0, 0, 0),
                    Y = c(-0.45, -0.25, 0, 0.25, 0.45),
                    Z = c(-0.75, 0.25, 1, 0, 1))

  AABB <- matrix(c(0, 0, 0), ncol = 3)
  edge_length <- c(1, 1, 1)

  AABB_min <- c(AABB[1, 1] - edge_length[1]/2,
                AABB[1, 2] - edge_length[2]/2,
                AABB[1, 3] - edge_length[3]/2)

  AABB_max <- c(AABB[1, 1] + edge_length[1]/2,
                AABB[1, 2] + edge_length[2]/2,
                AABB[1, 3] + edge_length[3]/2)



  test_0 <- line_AABB(orig[1,], end[1,], AABB_min, AABB_max)
  test_1 <- line_AABB(orig[2,], end[2,], AABB_min, AABB_max)
  test_2 <- line_AABB(orig[3,], end[3,], AABB_min, AABB_max)
  test_3 <- line_AABB(orig[4,], end[4,], AABB_min, AABB_max)
  test_4 <- line_AABB(orig[5,], end[5,], AABB_min, AABB_max)

  expect_equal(as.numeric(test_0[1]), 0, info = "code_0")
  expect_equal(as.numeric(test_0[2]), 0, info = "path_0")

  expect_equal(as.numeric(test_1[1]), 1, info = "code_0")
  expect_equal(as.numeric(test_1[2]), 0.5, info = "path_0")

  expect_equal(as.numeric(test_2[1]), 2, info = "code_0")
  expect_equal(as.numeric(test_2[2]), 0.5, info = "path_0")

  expect_equal(as.numeric(test_3[1]), 3, info = "code_0")
  expect_equal(as.numeric(test_3[2]), 0.5, info = "path_0")

  expect_equal(as.numeric(test_4[1]), 4, info = "code_0")
  expect_equal(as.numeric(test_4[2]), 1, info = "path_0")
})
