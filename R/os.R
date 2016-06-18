#' Generate OS check.
#'
#' \code{is_os_} returns a function to check the operating system in use.
#'
#' The intent is not to use this function directly.  Instead use one of the
#' functions
#' \itemize{
#'   \item \code{\link{is_linux}}
#'   \item \code{\link{is_osx}}
#'   \item \code{\link{is_windows}}
#' }
#'
#' @param os_string String containing the OS to check for.
#' Choices include
#' \itemize{
#'   \item \code{"os x"}
#'   \item \code{"linux"}
#'   \item \code{"windows"}
#' }
#'
#' @return A function that checks the type of OS.
#'
#' @examples
#'
#' @seealso
#' \href{http://adv-r.had.co.nz/Functional-programming.html#closures}{Closures}
is_os_ <- function(os_string) {
  function() {
    grepl(pattern = os_string, tolower(Sys.info()['sysname']), ignore.case = TRUE)
  }
}

#' Is Linux
#'
#' Is the OS Linux.
#'
#' @return A logical vector indicating whether the OS is Linux.
#'
#' @family is_os
#'
#' @export
is_linux <- is_os_("linux")


#' Is OSX
#'
#' Is the OS OSX.
#'
#' @return A logical vector indicating whether the OS is OSX.
#'
#' @family is_os
#'
#' @export
is_osx <- is_os_("darwin")


#' Is Windows
#'
#' Is the OS Windows.
#'
#' @return A logical vector indicating whether the OS is Windows.
#'
#' @family is_os
#'
#' @export
is_windows <- is_os_("windows")
