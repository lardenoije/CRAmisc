context("Dataframe Functions")

key <- c(1000L, 2000L, 3000L, 4000L, 1000L, 1000L)
key2 <- c(1001L, 2001L, 3001L, 4001L, 1001L, 1002L)
amount <- c("46.41", "118.11", "84.68", "493.59", "51.10", "88.49")

test_df <- data.frame(key, key2, amount, stringsAsFactors = FALSE)

## which keys are duplicated
dup_keys <- duplicate_keys(df = test_df, "key")
dup_keys_multiple <- duplicate_keys(df = test_df, "key", "key2")

# tests
test_that("duplicate keys are identified", {
  expect_equal(dup_keys, dplyr::as_data_frame(list(key = c(1000L),
                                                   n = c(3L))))
})

test_that("duplicate keys are identified with multiple key columns", {
  expect_equal(dup_keys_multiple,
               dplyr::as_data_frame(list(key = c(1000L),
                                         key2 = c(1001L),
                                         n = c(2L))))
})

test_that("match_cols returns columns", {
  test_col_patt <- "^am"
  expect_equal(match_cols(tbl = test_df,
                          patt = test_col_patt), "amount")
})

test_that("types_df returns the appropriate column name and type", {
  expect_equal(test_df %>% types_df(),
               data.frame(col_name = c("key", "key2", "amount"),
                          col_type = c("integer", "integer", "character"),
                          stringsAsFactors = FALSE) %>%
                 dplyr::as_data_frame())
})

## test tibble for case matching
test_df_case <- tibble::tribble(
  ~MixedCase, ~lowercase, ~UPPERCASE, ~aLtCaSe,
  1, 11, 111, 1111,
  2, 22, 222, 2222,
  3, 33, 333, 3333
)

## column names to search for, ignoring case
my_cols <- c("mixedcase", "altcase")

test_that("exact_cols returns appropriate case", {
  expect_equal(exact_cols(tbl = test_df_case,
                          cols = my_cols),
               c("MixedCase", "aLtCaSe"))
})

test_that("exact_cols can be used with a pipe", {
  exact_cols_pipeline <- function(df, cols) {
    df %>%
      exact_cols(cols)
  }

  expect_equal(exact_cols_pipeline(test_df_case, my_cols),
               c("MixedCase", "aLtCaSe"))
})

test_that("exact_cols works with dplyr::one_of", {
  result_df <- tibble::tribble(
    ~MixedCase, ~aLtCaSe,
    1, 1111,
    2, 2222,
    3, 3333
  )

  expect_identical(test_df_case %>%
                     dplyr::select(test_df_case %>%
                                   exact_cols(my_cols) %>%
                                   dplyr::one_of()),
                   result_df)
})
