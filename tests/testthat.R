Sys.setenv("R_TESTS" = "")

library(testthat)
library(rTLS)

test_check("rTLS")
