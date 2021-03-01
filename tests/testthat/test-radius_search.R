context("radius_search")

test_that("Whether radius_search works", {

  point_cloud <- data.table(X = c(0, 0, 0, 0, 0, -1, 1),
                            Y = c(0, 0, 0, -1, 1, 0, 0),
                            Z = c(-1, 0, 1, 0, 0, 0, 0))



  to_test <- radius_search(point_cloud, point_cloud, radius = 2, 2, TRUE, "kdtree")

  expect_equal(nrow(to_test[ref == 2]), 6, info = "radius_search")
  expect_equal(max(to_test$distance), 1, info = "value of distance")
  expect_equal(min(to_test$distance), 1, info = "value of distance")

  to_test <- radius_search(point_cloud, point_cloud, radius = 2, 7, TRUE, "kdtree")

  expect_equal(nrow(to_test[query == 2]), 6, info = "radius_search")
  expect_equal(max(to_test$distance), 1, info = "value of distance")
  expect_equal(min(to_test$distance), 1, info = "value of distance")
})
