Sys.setenv("R_TESTS" = "")

library(testthat)
library(recordSwapping)

test_check("recordSwapping")
