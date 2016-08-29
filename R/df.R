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
    dplyr::summarize_(key_count = dplyr::n()) %>%
    dplyr::filter_(key_count > 1)
}