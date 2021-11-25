### Artificial_stands

test_that("Whether random locations of trees works", {

  path <- system.file("extdata", "pc_tree.txt", package = "rTLS") ###Path for tree
  files <- rep(path, 2) #N trees

  to_test <- artificial_stand(files, n.trees = 2, dimension = c(30, 30), overlap = 0,
                              progress = FALSE, plot = FALSE, n_attempts = 10000)

  expect_equal(as.numeric(to_test$Stand[1,1]), 2, info = "Number of trees")
  expect_equal(as.numeric(to_test$Stand[1,2]), 900, info = "Stand area")
  expect_equal(round(as.numeric(to_test$Stand[1,4]), 2), 57.1, info = "Crown area")
  expect_equal(as.numeric(to_test$Stand[1,5]), 151624, info = "Number of points")
})

test_that("Whether the pre-difine locations of trees works", {

  path <- system.file("extdata", "pc_tree.txt", package = "rTLS") ###Path for tree
  files <- rep(path, 4) #N trees
  location <- data.table(X = c(5, 10, 10, 5), Y = c(5, 5, 10, 10))

  to_test <- artificial_stand(files, n.trees = 4, dimension = c(15, 15),
                              coordinates = location,
                              rotation = FALSE,
                              progress = FALSE, plot = FALSE)

  ###Stand information
  expect_equal(as.numeric(to_test$Stand[1,1]), 4, info = "Number of trees")
  expect_equal(as.numeric(to_test$Stand[1,2]), 225, info = "Stand area")
  expect_equal(round(as.numeric(to_test$Stand[1,3]), 2), 104.77, info = "Covered area")
  expect_equal(round(as.numeric(to_test$Stand[1,4]), 2), 104.77, info = "Crown area")
  expect_equal(as.numeric(to_test$Stand[1,5]), 303248, info = "Number of points")

  ###Trees information
  expect_equal(as.numeric(to_test$Trees[1,3]), 5, info = "X coordinate of first tree")
  expect_equal(as.numeric(to_test$Trees[1,4]), 5, info = "Y coordinate of first tree")
  expect_equal(as.numeric(to_test$Trees[4,3]), 5, info = "X coordinate of last tree")
  expect_equal(as.numeric(to_test$Trees[4,4]), 10, info = "Y coordinate of last tree")
  expect_equal(as.numeric(to_test$Trees[1,6]), 6.036, info = "Hight of a tree")
  expect_equal(round(as.numeric(to_test$Trees[1,5]), 2), 28.55, info = "CA of a tree")

  ###Cloud information
  expect_equal(nrow(to_test$Cloud), 303248, info = "N of points")
  expect_equal(unique(to_test$Cloud$Tree), 1:4, info = "ID of trees")
})
