# setting R_TESTS to empty string because of
# https://github.com/hadley/testthat/issues/144
# revert this when that issue in R is fixed.
Sys.setenv("R_TESTS" = "")
Sys.setlocale('LC_ALL','C')

library(testthat)
library(tosca)

test_check("tosca")
