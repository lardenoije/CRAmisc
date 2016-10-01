context("Convert Columns and Specification")

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

types_df <- tibble::tribble(
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

types_df_missing_col <- tibble::tribble(
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

types_df_not_tibble <- data.frame(
  col_name = c("date_var",
               "datetime_var",
               "double_var1",
               "double_var2",
               "numeric_var",
               "logical_var",
               "character_var",
               "integer_var"),
  col_type = c("date",
               "datetime",
               "double",
               "double",
               "numeric",
               "logical",
               "character",
               "integer")
)

# convert the columns - missing column
suppressMessages(test_df_converted_missing_col <- convert_cols(test_df,
                                                               types_df_missing_col))

# convert the columns - all columns
suppressMessages(test_df_converted <- convert_cols(test_df, types_df))

# convert the columns - all columns
suppressMessages(test_df_converted_not_tibble <- convert_cols(test_df,
                                                              types_df_not_tibble))

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

test_that("conversion happens if types_df is not a tibble", {
  expect_identical(test_df_converted_not_tibble, test_df_converted)
})


## spec tests
csv_df <- readr::read_csv(paste0("a,b,c,d\n",
                                 "1,two,3.0,2016-05-01T11:40:44\n",
                                 "4,five,6.0,2014-12-01T06:12:23"))
csv_spec <- readr::spec(csv_df)

## update columns a and c
col_spec_df <- tibble::tribble(
  ~col_name, ~col_type,
  "a", "double",
  "c", "character"
)

## update the specification
csv_spec_updated <- spec_update(csv_spec, col_spec_df)

## re-read with new column spec
csv_updated <- readr::read_csv(paste0("a,b,c,d\n",
                                      "1,two,3.0,2016-05-01T11:40:44\n",
                                      "4,five,6.0,2014-12-01T06:12:23"),
                               col_types = csv_spec_updated)

test_that("column types were updated appropriately", {
  expect_type(csv_updated$a, "double")
})

## spec_to_df
test_that("spec can be converted to a dataframe", {
  # returns a dataframe
  expect_true(is.data.frame(spec_to_df(csv_spec)))

  # gets the appropriate column type
  csv_spec_col_a <- spec_to_df(csv_spec) %>%
    dplyr::filter(col_name == "a") %>%
    .$col_type
  expect_equal(class(csv_df$a), csv_spec_col_a)
})

## spec_from_df
test_that("spec can be converted from a dataframe", {
  spec_df <- tibble::tribble(
    ~col_name, ~col_type,
    "a", "integer",
    "b", "character",
    "c", "double",
    "d", "datetime"
  )
  csv_spec_from_df <- spec_from_df(spec_df)

  # returns a specification
  expect_is(csv_spec_from_df, "col_spec")

  # returns the appropriate class
  col_a <- csv_spec_from_df %>%
    .$cols %>%
    .$a

  expect_is(col_a, c("collector_integer", "collector"))
})
