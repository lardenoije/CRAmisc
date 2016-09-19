context("Is OS")

# tests
test_that("is_os returns logical", {
  expect_type(is_linux(), "logical")
  expect_type(is_osx(), "logical")
  expect_type(is_windows(), "logical")
})

test_that("is_windows returns true", {
  skip_on_os(c("linux", "mac", "solaris"))
  expect_equal(is_windows(), TRUE)
})

test_that("is_osx returns true", {
  skip_on_os(c("linux", "windows", "solaris"))
  expect_equal(is_osx(), TRUE)
})

test_that("is_linux returns true", {
  skip_on_os(c("mac", "windows", "solaris"))
  expect_equal(is_linux(), TRUE)
})
