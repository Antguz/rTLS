### Geometry_features

test_that("Whether geometry features works", {

  point_cloud <- data.table(X = c(0, 0, 0, 0, 0, -1, 1),
                            Y = c(0, 0, 0, -1, 1, 0, 0),
                            Z = c(-1, 0, 1, 0, 0, 0, 0))

  k <- c(5, 7)

  to_test_knn <- geometry_features(point_cloud, method = "knn", k = k)

  expect_equal(dim(to_test_knn), c(7, 3, 2), label = "knn dimensions")

  # Keep your sum-based checks, but allow tiny numeric differences across platforms
  expect_equal(sum(round(to_test_knn[,1,1], 4)), 3.8336, tolerance = 1e-4, label = "value of PC1")
  expect_equal(sum(round(to_test_knn[,2,1], 4)), 2.1668, tolerance = 1e-4, label = "value of PC2")
  expect_equal(sum(round(to_test_knn[,3,1], 4)), 1.0002, tolerance = 1e-4, label = "value of PC3")

  expect_equal(sum(round(to_test_knn[,1,2], 4)), 2.3331, tolerance = 1e-4, label = "value of PC1")
  expect_equal(sum(round(to_test_knn[,2,2], 4)), 2.3331, tolerance = 1e-4, label = "value of PC2")
  expect_equal(sum(round(to_test_knn[,3,2], 4)), 2.3331, tolerance = 1e-4, label = "value of PC3")

  radius <- c(1, 5)

  to_test_radius <- geometry_features(point_cloud, method = "radius_search",
                                      radius = radius, max_neighbour = 7, progress = FALSE)

  expect_equal(dim(to_test_radius), c(7, 4, 2), label = "radius dimensions")
  expect_equal(sum(round(to_test_radius[,1,1], 4)), 19, tolerance = 1e-8, label = "npoints")
  expect_true(any(is.nan(to_test_radius[,2,1])), label = "value of PC1 contains NaN")
  expect_true(any(is.nan(to_test_radius[,3,1])), label = "value of PC2 contains NaN")
  expect_true(any(is.nan(to_test_radius[,4,1])), label = "value of PC3 contains NaN")

})
