#' One-way frequency table.
#'
#' Create a simple one-way frequency table using \code{dplyr} verbs.
#'
#' @param df The dataframe to perform the frequency on.
#' @param ... The column(s) to \code{group by} for counting.
#'
#' @return A \link{tibble}[tibble] containing the keys and a count of
#' occurrences.
#'
#' @examples
#' iris %>% freq("Species")
#'
#' @export
freq <- function(df, ...) {
  df %>%
    dplyr::group_by_(...) %>%
    dplyr::tally()
}