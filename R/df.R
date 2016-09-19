#' Find duplicate keys in a dataframe.
#'
#' Return a vector of the duplicate keys within a dataframe.  Filters out NA
#' values before determining duplicates.
#'
#' @param df The dataframe to check for duplicate keys.
#' @param key_col The column to check for duplicates.
#'
#' @return A tibble containing the duplicate keys and a count of the number of
#' duplicates.
#'
#' @examples
#' ## dataframe to convert
#' key <- c(1000L, 2000L, 3000L, 4000L, 1000L)
#' amount <- c("46.41", "118.11", "84.68", "493.59", "51.10")
#'
#' test_df <- data.frame(key, amount)
#'
#' ## which keys are duplicated
#' duplicate_keys(df = test_df, key_col = "key")
#'
#' @export
duplicate_keys <- function(df, key_col) {
  df %>%
    dplyr::group_by_(key_col) %>%
    dplyr::tally() %>%
    dplyr::filter_(~n > 1)
}


#' Match column name.
#'
#' \code{match_cols} returns matching column names from a table.  The matching
#' is performed using regular expressions and \link[base]{grepl}.
#'
#' Ignores case by default.  Can be overridden by passing \code{ignore.case} via
#' \code{...}.
#'
#' @param tbl A \link[tibble]{tibble} or dataframe.
#' @param patt Regular expression pattern to pass to \link[base]{grepl}.
#' @param ... Parameters to pass along to \link[base]{grepl}.
#'
#' @return A character vector of column names matching the regex.
#'
#' @examples
#' ## dataframe to convert
#' key <- c(1000L, 2000L, 3000L, 4000L, 1000L)
#' amount <- c("46.41", "118.11", "84.68", "493.59", "51.10")
#'
#' test_df <- data.frame(key, amount)
#'
#' # column names
#' my_col <- "^am"
#'
#' # use
#' test_df %>%
#'   match_cols(my_col)
#'
#' @export
match_cols <- function(tbl, patt, ...) {
    cols <- dplyr::tbl_vars(tbl)
    purrr::keep(.x = cols,
                .p = grepl(patt, cols, ignore.case = TRUE, ...))
}


#' List exact names of columns.
#'
#' \code{exact_cols} returns the exact column names, including the case.
#'
#' This particularly useful when working with Microsoft SQL Server tables as
#' the column names may be case sensitive.  This is also useful when working
#' with table metadata and the exact column names are required for matching
#' purposes.
#'
#' \link[dplyr]{tbl_vars} does not return case sensitive columns, thus the
#' the need for this function.
#'
#' @param tbl A \link[tibble]{tibble} or dataframe.
#' @param cols A character vector of column names.
#'
#' @return A character vector of column names with the appropriate case.
#'
#' @examples
#' ## create tibble
#' my_tibble <- tibble::tribble(
#'   ~MixedCase, ~lowercase, ~UPPERCASE, ~aLtCaSe,
#'   1, 11, 111, 1111,
#'   2, 22, 222, 2222,
#'   3, 33, 333, 3333
#' )
#'
#' ## column names to search for, ignoring case
#' my_cols <- c("mixedcase", "altcase")
#'
#' my_cols_case <- my_tibble %>%
#'   exact_cols(my_cols)
#'
#' ## particularly useful when combined with dplyr::one_of
#' my_tibble %>%
#'   dplyr::select(dplyr::one_of(my_cols_case))
#'
#' @export
exact_cols <- function(tbl, cols) {
  purrr::map_chr(
    cols,
    function(col) dplyr::tbl_vars(tbl)[match(tolower(col),
                                       tolower(dplyr::tbl_vars(tbl)))]
  )
}


#' Column names and types in a dataframe.
#'
#' Given a dataframe, \code{types_df} returns a \link[tibble]{tibble} with the
#' column names and types.
#'
#' @param df A dataframe.
#'
#' @return A \link[tibble]{tibble} with the column names and types.
#'
#' @examples
#' (mtcars %>%
#'    types_df())
#'
#' @export
types_df <- function(df) {
  df %>%
    purrr::dmap(class) %>%
    tidyr::gather(key = "col_name", value = "col_type")
}
