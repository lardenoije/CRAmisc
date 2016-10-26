#' Apply a function to each row of a database result set.
#'
#' \code{chunked_pmap} pulls chunks from a database result set and
#' applies a function to each row of the result set.  All transformed chunks
#' are row bound to return a single dataframe.
#'
#' This function is useful when processing database result sets that are
#' extremely large.  For example, if XML docs, JSON docs, or blobs of any kind
#' are stored in a database column, then \code{chunked_pmap} can be
#' used to iteratively process and transform the data.
#'
#' @param res \code{DBI::dbSendQuery} result set.
#' @param f The function that will be applied to each row of the database result
#' set.  \code{f} is assumed to be constructed using a \code{dplyr} pipeline
#' where a dataframe is passed as the sole parameter and a transformed dataframe
#' is returned.
#' @param n The number of rows to pull back from the database result set for
#' each chunk.
#' @param gc The type of garbage collection to run.  Default is \code{NA} or no
#' garbage collection.  Choices include
#' \itemize{
#'   \item \code{r} = R garbage collection
#'   \item \code{j} = Java garbage collection
#'   \item \code{rj} = R and Java garbage collection
#' }
#'
#' @return A dataframe that has been transformed by \code{f}.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[purrr]{pmap}}
#'   \item \code{\link[dplyr]{rowwise}}
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
#' mtcars_new <- chunked_pmap(res, f, 5)
#'
#' # cleanup
#' DBI::dbClearResult(res)
#' DBI::dbDisconnect(dbcon)
#' }
#'
#' @export
chunked_pmap <- function(res, f, n, gc = NA_character_) {
  # setup gc
  if(is.na(gc)) {
    gc_func <- function() { NULL }
  } else if(gc == "r") {
    gc_func <- function() { gc() }
  } else if(gc == "j") {
    gc_func <- function() { rJava::.jcall("java/lang/System", method = "gc") }
  } else if(gc %in% c("rj", "jr")) {
    gc_func <- function() {
      gc()
      rJava::.jcall("java/lang/System", method = "gc")
    }
  } else {
    stop("The gc option should be set to\n'r'  ==> R garbage collection\n'j'  ==> Java garbage collection\n'rj' ==> R and Java garbage collection", call. = FALSE)
  }

  # an expandingList is a fast implementation of a list that can be
  #   dynamically appended to and turned into a standard R list
  df_list <- expandingList()

  while (!DBI::dbHasCompleted(res)) {
    df_chunk <- DBI::dbFetch(res, n = n)  # dataframe
    if(nrow(df_chunk) != 0) {
      df_res <- f(df_chunk)  # apply function
    } else {
      break
    }
    df_list$add(df_res)  # add to expandingList
    gc_func()   # cleanup
  }
  # convert to an actual list as opposed to an expandingList
  df_list <- df_list$as.list()

  # clear results
  invisible(DBI::dbClearResult(res))

  # final step is to bind into a single dataframe
  dplyr::bind_rows(df_list)
}
