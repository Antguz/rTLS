context("truck_volumen")

test_that("Test whether the truck_volumen works", {

  data("pc_tree")

  to_test <- trunk_volume(pc_tree, max.height = 1.75, plot = FALSE)

  expect_equal(as.numeric(round(to_test, 4)), 0.0717, info = "Trunk volume")

})
