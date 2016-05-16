#' Fast expanding list.
#'
#' \code{expandingList} is a fast implementation of a list that can be appended
#' to and turned into a list.
#'
#' \code{expandingList} is a
#' \href{http://adv-r.had.co.nz/Functional-programming.html#closures}{closure}
#' that enables modification in place.  It serves as an
#' \href{http://www.win-vector.com/blog/2015/03/using-closures-as-objects-in-r/)}{object}
#' in R.
#'
#' The code is copied from Jan Kanis at
#' \url{http://stackoverflow.com/a/32870310}.
#'
#' @param capacity Initial list capacity or length.
#'
#' @return A closure named expandingList with functions (or methods)
#' \itemize{
#'   \item \code{expandingList$double.size} doubles the size of the list
#'   \item \code{expandingList$add} adds an element to the list
#'   \item \code{expandingList$as.list} converts the \code{expandingList} into
#'   an R list
#' }
#'
#' @examples
#' g <- expandingList(5)  ## expandingList of length 5
#'
#' for (i in 1:10) g$add(i)  ## append to list without explicitly expanding
#'
#' identical(1:10, unlist(g$as.list()))  ## TRUE
#'
#' @export
expandingList <- function(capacity = 10, envir = emptyenv()) {
  buffer <- vector('list', capacity)
  length <- 0

  methods <- list()

  # <<- if buffer exists in this
  methods$double.size <- function() {
    buffer <<- c(buffer, vector('list', capacity))
    capacity <<- capacity * 2
  }

  methods$add <- function(val) {
    if(length == capacity) {
      methods$double.size()
    }

    length <<- length + 1
    buffer[[length]] <<- val
  }

  methods$as.list <- function() {
    b <- buffer[0:length]
    return(b)
  }

  methods
}
