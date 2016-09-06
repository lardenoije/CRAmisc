context("Dataframe Functions")

key <- c(1000L, 2000L, 3000L, 4000L, 1000L)
amount <- c("46.41", "118.11", "84.68", "493.59", "51.10")

test_df <- data.frame(key, amount)

## which keys are duplicated
dup_keys <- duplicate_keys(df = test_df, key_col = "key")

# tests
test_that("duplicate keys are identified", {
  expect_equal(dup_keys, dplyr::as_data_frame(list(key = c(1000L),
                                                   n = c(2L))))
})

test_that("has_cols returns columns", {
  test_col_patt <- "^am"
  expect_equal(has_cols(tbl = test_df,
                        patt = test_col_patt), "amount")
})