### Geometry_features

test_that("Whether geometry features works", {

  point_cloud <- data.table(X = c(0, 0, 0, 0, 0, -1, 1),
                            Y = c(0, 0, 0, -1, 1, 0, 0),
                            Z = c(-1, 0, 1, 0, 0, 0, 0))

  k <- c(5, 7)

  to_test_knn <- geometry_features(point_cloud, method = "knn", k = k)

  expect_equal(dim(to_test_knn), c(7, 3, 2), info = "knn dimensions")
  expect_equal(sum(round(to_test_knn[,1,1], 4)), 3.8336, info = "value of PC1")
  expect_equal(sum(round(to_test_knn[,2,1], 4)), 2.1668, info = "value of PC2")
  expect_equal(sum(round(to_test_knn[,3,1], 4)), 1.0002, info = "value of PC3")

  expect_equal(sum(round(to_test_knn[,1,2], 4)), 2.3331, info = "value of PC1")
  expect_equal(sum(round(to_test_knn[,2,2], 4)), 2.3331, info = "value of PC2")
  expect_equal(sum(round(to_test_knn[,3,2], 4)), 2.3331, info = "value of PC3")

  radius <- c(1, 5)

  to_test_radius <- geometry_features(point_cloud, method = "radius_search", radius = radius, max_neighbour = 7, progress = FALSE)

  expect_equal(dim(to_test_radius), c(7, 4, 2), info = "radius dimensions")
  expect_equal(sum(round(to_test_radius[,1,1], 4)), 19, info = "npoints")
  expect_equal(sum(round(to_test_radius[,2,1], 4)), NaN, info = "value of PC1")
  expect_equal(sum(round(to_test_radius[,3,1], 4)), NaN, info = "value of PC2")
  expect_equal(sum(round(to_test_radius[,4,1], 4)), NaN, info = "value of PC3")

})
