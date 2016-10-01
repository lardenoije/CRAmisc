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

test_df2 <- tibble::tribble(
  ~a, ~b, ~c, ~d,
  1,"two","2016-01-11T11:00:14", 8.0,
  3,"four","2012-03-08T18:48:11", 94.39,
  5,"six","2010-10-10T01:01:01", 7,
  7,"eight","2000-04-01T08:24:41", 19.48
)

test_df2 <- test_df2 %>%
  purrr::dmap_at(.at = "c",
                 .f = function(x) lubridate::as_datetime(x)) %>%
  dplyr::mutate(e = lubridate::as_date(c))

test_that("types_df handles datetime columns", {
  expect_equal(test_df2 %>% types_df() %>% nrow(), 5)
  expect_equal(test_df2 %>%
                 types_df() %>%
                 dplyr::filter(col_name == "c") %>%
                 .$col_type, "datetime")
  expect_equal(test_df2 %>%
                 types_df() %>%
                 dplyr::filter(col_name == "d") %>%
                 .$col_type, "numeric")
  expect_equal(test_df2 %>%
                 types_df() %>%
                 dplyr::filter(col_name == "e") %>%
                 .$col_type, "date")
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
