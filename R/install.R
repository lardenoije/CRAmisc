#' Check and install packages.
#'
#' \code{check_install} checks if a package is already installed.  If it is not
#' installed, then install the package (from CRAN).
#'
#' @param pkgs List or vector of packages to check and optionally install.
#' @param repos Character vector of the base URL(s) of the repositories to use,
#' e.g., the URL of a CRAN mirror.
#' @param ... List of options to pass install.packages.
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
check_install <- function(pkgs, repos = NULL, ...) {
  installed_packages <- installed.packages()[ ,1]
  for (i in seq_along(pkgs)) {
    pkg <- pkgs[[i]]
    if (!pkg %in% installed_packages) {
      cat(paste0("Need to install: ", pkg, "\n"))
      if(is.null(repos)) install.packages(pkg, repos = "https://cran.rstudio.com")
      else install.packages(pkg, repos = repos, ...)
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
#' @param cainfo Character vector of the file path to a CA file, e.g.,
#' ca-bundle.crt.
#' @param ... List of options to pass install.packages.
#'
#' @return There is no return as the function is called for its side effects.
#'
#' @examples
#' \dontrun{
#' prior_install(pkg = "pkgKitten", pkg_version = "0.1.0")
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
prior_install <- function(pkg, pkg_version, cainfo = NULL, ...) {
  pkg_url <- paste0("https://cran.rstudio.com/src/contrib/Archive/",
                    pkg, "/", pkg, "_", pkg_version, ".tar.gz")

  safe_http_error <- purrr::safely(httr::http_error)

  if(!is.null(cainfo)) {
    httr::set_config(httr::config(cainfo = cainfo))
    pkg_url_error <- safe_http_error(pkg_url)
    httr::reset_config()
  } else {
    pkg_url_error <- safe_http_error(pkg_url)
  }

  if(is.null(pkg_url_error$result)) {
    stop(paste0("The url ", pkg_url, " returned an HTTP error:\n",
                pkg_url_error$error))
  } else {
    install.packages(pkg_url, repos = NULL,
                     type = "source",
                     ...)
  }
}


#' Packages Dataframe.
#'
#' \code{pkgs_df} returns a \link[tibble]{tibble} containing a subset of
#' information about the packages installed.
#'
#' @return Returns a \href{https://cran.rstudio.com/package=tibble}{tibble}
#' containing a subset of information about the packages installed.
#'
#' @examples
#' pkgs <- pkgs_df()
#' pkgs
#'
#' @seealso
#' \link{check_install}
#'
#' @importFrom utils installed.packages
#'
#' @export
pkgs_df <- function() {
  installed.packages() %>%
    as.data.frame(stringsAsFactors=FALSE) %>%
    dplyr::select_("Package", "Version", "Depends", "Imports", "LibPath", "Built") %>%
    dplyr::distinct_("Package", "Version", "Depends", "Imports", "LibPath", "Built") %>%
    dplyr::arrange_("Package") %>%
    dplyr::as_data_frame()
}
