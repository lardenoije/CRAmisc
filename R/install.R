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
#' \itemize{
#'   \item \href{https://rstudio.github.io/packrat/}{Packrat}
#'   \item \href{https://github.com/robertzk/lockbox}{Lockbox}
#' }
#'
#' @importFrom utils install.packages installed.packages
#'
#' @export
check_install <- function(pkgs, repos = NULL) {
  installed_packages <- installed.packages()[ ,1]
  for (i in seq_along(pkgs)) {
    pkg <- pkgs[[i]]
    if (!pkg %in% installed_packages) {
      cat(paste0("Need to install: ", pkg, "\n"))
      if(is.null(repos)) install.packages(pkg, repos = "https://cran.rstudio.com")
      else install.packages(pkg, repos = repos)
    } else cat(paste0(pkg, " already installed\n"))
  }
  invisible(pkgs)
}


#' Install prior version of a package.
#'
#' \code{prior_install} will install a prior version of a package from source.
#' \code{prior_install} will first check that the version supplied exists by
#' querying for the tar.gz file from the
#' \href{https://cran.rstudio.com/src/contrib/Archive}{CRAN archive}.  If the
#' \code{tar.gz} file exists, it will attempt to install it.
#'
#' @param pkg Character vector of the package to install.
#' @param pkg_version Character vector of the package version to install.
#'
#' @return There is no return as the function is called for its side effects.
#'
#' @examples
#' \dontrun{
#' prior_install(pkg = "tidytext", pkg_version = "0.1.0")
#' }
#'
#' @seealso
#' \itemize{
#'   \item \link{check_install}
#'   \item \href{https://rstudio.github.io/packrat/}{Packrat}
#'   \item \href{https://github.com/robertzk/lockbox}{Lockbox}
#' }
#'
#' @importFrom utils install.packages installed.packages
#'
#' @export
prior_install <- function(pkg, pkg_version) {
  pkg_url <- paste0("https://cran.rstudio.com/src/contrib/Archive/",
                    pkg, "/", pkg, "_", pkg_version, ".tar.gz")

  pkg_url_error <- httr::http_error(pkg_url)

  if(pkg_url_error) {
    stop(paste0("The url ", pkg_url, " returns an HTTP error."))
  } else {
    install.packages(pkg_url, repos = NULL, type = "source")
  }
}
