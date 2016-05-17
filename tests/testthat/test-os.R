context("Is OS")

# tests
test_that("is_os returns logical", {
  expect_type(is_linux(), "logical")
  expect_type(is_osx(), "logical")
  expect_type(is_windows(), "logical")
})
