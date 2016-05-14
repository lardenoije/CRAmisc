#' Check and install packages.
#'
#' \code{check_install} checks if a package is already installed.  If it is not
#' installed, then install the package (from CRAN).
#'
#' @param pkgs List or vector of packages to check and optionally install.
#'
#' @return Invisibly returns the input pkgs list or vector.  The function is
#' called for its side effects.
#'
#' @examples
#' req_pkgs <- list(
#'   "dplyr",
#'   "purrr",
#'   "rprojroot"
#' )
#'
#' check_install(req_pkgs)
#'
#' @seealso
#'   \itemize{
#'     \item \href{https://rstudio.github.io/packrat/}{Packrat}
#'     \item \href{https://github.com/robertzk/lockbox}{Lockbox}
#'   }
#'
#' @export
check_install <- function(pkgs) {
  installed_packages <- installed.packages()[,1]
  for (i in seq_along(pkgs)) {
    pkg <- pkgs[[i]]
    if (!pkg %in% installed_packages) {
      cat(paste0("Need to install: ", pkg, "\n"))
      install.packages(pkg)
    } else cat(paste0(pkg, " already installed\n"))
  }
  invisible(pkgs)
}