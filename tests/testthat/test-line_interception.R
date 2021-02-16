context("line_interception")
test_that("Test whether lines_interception works", {

  n <- 20
  set.seed(07)
  orig <- data.table(X = runif(n, min = -5, max = 5),
                     Y = runif(n, min = -5, max = 5),
                     Z = runif(n, min = -5, max = 5))

  set.seed(10)
  end <- data.table(X = runif(n, min = -5, max = 5),
                    Y = runif(n, min = -5, max = 5),
                    Z = runif(n, min = -5, max = 5))

  AABBs <- data.table(X = 0, Y = 0, Z = 0)
  edge_length <- c(2, 2, 2)

  test_1 <- lines_interception(orig, end, AABBs, edge_length, progress = FALSE)

  #Outsite
  expect_equal(test_1$code_0[1], 16, info = "code_0")
  expect_equal(test_1$code_4[1], 4, info = "code_4")
  expect_equal(round(test_1$path_4[1], 2), 1.77, info = "path_4")


  #Inside
  edge_length <- c(5, 5, 5)
  test_2 <- lines_interception(orig, end, AABBs, edge_length, progress = FALSE)

  expect_equal(test_2$code_0[1], 8, info = "code_0")
  expect_equal(test_2$code_1[1], 0, info = "code_1")
  expect_equal(test_2$code_2[1], 3, info = "code_2")
  expect_equal(test_2$code_3[1], 4, info = "code_3")
  expect_equal(test_2$code_4[1], 5, info = "code_4")

  expect_equal(round(test_2$path_1[1], 2), 0, info = "path_4")
  expect_equal(round(test_2$path_2[1], 2), 13.36, info = "path_4")
  expect_equal(round(test_2$path_3[1], 2), 17.64, info = "path_4")
  expect_equal(round(test_2$path_4[1], 2), 13.05, info = "path_4")
})
