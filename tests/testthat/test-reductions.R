context("Reductions")

test_that("freq returns correct count", {
  setosa_count <- iris %>%
    freq("Species") %>%
    dplyr::filter(Species == "setosa") %>%
    .$n

  expect_equal(setosa_count, 50)
})