context("Data Structures")

el <- expandingList(5)
for (i in 1:10) {
  el$add(i)
}

# tests
test_that("expandingList dynamically expands", {
  expect_identical(unlist(el$as.list()), 1:10)
})

test_that("expandingList can be converted to a list", {
  expect_type(el$as.list(), "list")
})
