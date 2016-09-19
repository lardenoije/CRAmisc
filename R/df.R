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


#' Matches a column name?
#'
#' \code{match_cols} returns matching column names from a database table.  The
#' matching is performed using regular expressions and \link[base]{grepl}.
#'
#' Ignores case by default.  Can be overridden by passing \code{ignore.case} via
#' \code{...}.
#'
#' @param tbl A \link[tibble]{tibble}.
#' @param patt Regular expression pattern to pass to \code{grepl}.
#' @param ... Parameters to pass along to \code{grepl}.
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