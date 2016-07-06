library(testthat)
library(CRAmisc)

Sys.setenv("R_TESTS" = "")
test_check("CRAmisc")
