### knn

test_that("Whether knn works", {

  point_cloud <- data.table(X = c(0, 0, 0, 0, 0, -1, 1),
                            Y = c(0, 0, 0, -1, 1, 0, 0),
                            Z = c(-1, 0, 1, 0, 0, 0, 0))



  to_test <- knn(point_cloud, point_cloud, 2, same = TRUE, "kdtree", checks = 10)

  expect_equal(nrow(to_test[ref == 2]), 6, info = "knn")
  expect_equal(max(to_test$distance), 2, info = "value of distance")
  expect_equal(min(to_test$distance), 1, info = "value of distance")

  to_test <- knn(point_cloud, point_cloud, 2, same = FALSE, "kdtree", checks = 10)

  expect_equal(nrow(to_test[ref == 2]), 7, info = "knn")
  expect_equal(max(to_test$distance), 1, info = "value of distance")
  expect_equal(min(to_test$distance), 0, info = "value of distance")
})
