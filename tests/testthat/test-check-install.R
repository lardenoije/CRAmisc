context("Check install")

pkgs_vector <- c("utils", "tools")
pkgs_list <- list("utils", "tools")

# tests
test_that("check_install returns message for already installed package", {
  expect_output(check_install("utils"), "utils already installed")
})

test_that("check_install can process a vector", {
  expect_output(check_install(pkgs_vector),
                "utils already installed\ntools already installed")
})

# test_that("check_install installs a missing package", {
#
# })