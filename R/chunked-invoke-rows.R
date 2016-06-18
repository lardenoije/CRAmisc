#' Apply a function to each row of a database result set.
#'
#' \code{chunked_invoke_rows} pulls chunks from a database result set and
#' applies a function to each row of the result set.  All transformed chunks
#' are row bound to return a single dataframe.
#'
#' This function is useful when processing database result sets that are
#' extremely large.  For example, if XML docs, JSON docs, or blobs of any kind
#' are stored in a database column, then \code{chunked_invoke_rows} can be
#' used to iteratively process and transform the data.
#'
#' @param res DBI::dbSendQuery result set.
#' @param f The function that will be applied to each row of the database result
#' set.  \code{f} is assumed to be constructed using a \code{dplyr} pipeline
#' where a dataframe is passed as the sole parameter and a transformed dataframe
#' is returned.
#' @param n The number of rows to pull back from the database result set for
#' each chunk.
#'
#' @return A dataframe that has been transformed by \code{f}.
#'
#' @seealso
#' \itemize{
#'   \item \code{purrr::invoke_rows}
#'   \item \code{dplyr::rowwise}
#'   \item The \href{https://github.com/edwindj/chunked}{chunked} package.
#' }
#'
#' @examples
#' \dontrun{
#' # create in-memory SQLite database
#' dbcon <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
#' DBI::dbWriteTable(dbcon, "mtcars", mtcars)
#'
#' # create and send a query
#' sql_query <- "SELECT * FROM mtcars where hp > 120"
#' res <- DBI::dbSendQuery(dbcon, sql_query)
#'
#' # create a function to operate on chunks
#' # the function should accept a dataframe and return a dataframe
#' f <- function(df) {
#'   df %>%
#'     dplyr::mutate(hp_to_cyl = hp / cyl) %>%
#'     dplyr::arrange(hp_to_cyl)
#' }
#'
#' # process 5 rows at a time
#' mtcars_new <- chunked_invoke_rows(res, f, 5)
#'
#' # cleanup
#' DBI::dbClearResult(res)
#' DBI::dbDisconnect(dbcon)
#' }
#'
#' @export
chunked_invoke_rows <- function(res, f, n) {
  if(!requireNamespace("DBI", quietly = TRUE)) {
    stop("The DBI package is needed for this function.  Please install it.",
         call. = FALSE)
  }
  # an expandingList is a fast implementation of a list that can be
  #   dynamically appended to and turned into a standard R list
  df_list <- expandingList()

  while (!DBI::dbHasCompleted(res)) {
    df_chunk <- DBI::dbFetch(res, n = n)  # dataframe
    df_res <- f(df_chunk)  # apply function
    df_list$add(df_res)  # add to expandingList
  }
  # convert to an actual list as opposed to an expandingList
  df_list <- df_list$as.list()

  # final step is to bind into a single dataframe
  dplyr::bind_rows(df_list)
}
