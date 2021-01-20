Sys.setenv("R_TESTS" = "")

library(testthat)
library(rTLS)

rgdal::set_thin_PROJ6_warnings(FALSE)
test_check("rTLS")
