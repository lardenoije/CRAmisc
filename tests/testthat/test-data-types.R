context("Convert Columns")

# dataframe to convert
id <- c(1L, 2L, 3L)
date_var <- c("2016-01-02", "2015-12-24", "2016-05-05")
datetime_var <- c("2016-01-02T11:11:04",
                  "2015-12-24T08:11:41",
                  "2016-05-05T05:05:49")
double_var1 <- c("46.41", "118.11", "84.68")
double_var2 <- c("68.48", "-248.99", "194")
numeric_var <- c("78.61", "593.1", "123")
logical_var <- c("true", "true", "false")
character_var <- c(49, 88, 104)
integer_var <- c("77", "84", "4949")

test_df <- data.frame(id,
                      date_var,
                      datetime_var,
                      double_var1,
                      double_var2,
                      numeric_var,
                      logical_var,
                      character_var,
                      integer_var, stringsAsFactors = FALSE)

types_df <- tibble::frame_data(
  ~col_name, ~col_type,
  "date_var", "date",
  "datetime_var", "datetime",
  "double_var1", "double",
  "double_var2", "double",
  "numeric_var", "numeric",
  "logical_var", "logical",
  "character_var", "character",
  "integer_var", "integer"
)

types_df_missing_col <- tibble::frame_data(
  ~col_name, ~col_type,
  "date_var", "date",
  "datetime_var", "datetime",
  "double_var1", "double",
  "double_var2", "double",
  "numeric_var", "numeric",
  "logical_var", "logical",
  "character_var", "character",
  "integer_var", "integer",
  "not_in_df1", "double",
  "not_in_df2", "double"
)

test_df_converted_missing_col <- convert_cols(test_df,
                                              types_df_missing_col)

# suppressMessages(test_df_converted_missing_col <- convert_cols(test_df,
#                                                               types_df_missing_col))

# convert the columns
suppressMessages(test_df_converted <- convert_cols(test_df, types_df))

# tests
test_that("date columns are converted", {
  expect_s3_class(test_df_converted$date_var,
                  "Date")
})

test_that("datetime columns are converted", {
  expect_s3_class(test_df_converted$datetime_var,
                  c("POSIXct", "POSIXt"))
})

test_that("double columns are converted", {
  expect_is(test_df_converted$double_var1,
            "numeric")
})

test_that("numeric columns are converted", {
  expect_is(test_df_converted$numeric_var,
            "numeric")
})

test_that("logical columns are converted", {
  expect_is(test_df_converted$logical_var,
            "logical")
})

test_that("character columns are converted", {
  expect_is(test_df_converted$character_var,
            "character")
})

test_that("integer columns are converted", {
  expect_is(test_df_converted$integer_var,
            "integer")
})

test_that("column not in df is skipped", {
  expect_false(tibble::has_name(test_df, "not_in_df"))
  expect_identical(test_df_converted_missing_col, test_df_converted)
})