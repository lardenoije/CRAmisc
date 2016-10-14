#' Pipe operator.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' Matches.
#'
#' Infix operator to check if a character vector matches an \code{R} regular
#' expression.
#'
#' @param lhs The character vector to search; the haystack.
#' @param rhs The \code{R} regular expression; the needle.
#'
#' @return Logical vector indicating a match.
#'
#' @examples
#' virginica_df <- iris %>%
#'   dplyr::filter(Species %matches% "^vir")
#'
#' @name %matches%
#' @rdname matches
#' @export
`%matches%` <- function(lhs, rhs) {
  grepl(rhs, lhs, ignore.case = TRUE)
}

#' Perl Matches.
#'
#' Infix operator to check if a character vector matches a \code{perl} regular
#' expression.
#'
#' @param lhs The character vector to search; the haystack.
#' @param rhs The \code{perl} regular expression; the needle.
#'
#' @return Logical vector indicating a match.
#'
#' @examples
#' virginica_df <- iris %>%
#'   dplyr::filter(Species %pmatches% "^vir")
#'
#' @name %pmatches%
#' @rdname pmatches
#' @export
`%pmatches%` <- function(lhs, rhs) {
  grepl(rhs, lhs, ignore.case = TRUE, perl = TRUE)
}

# prevent R CMD check from creating a NOTE when utilizing "." in a pipeline
utils::globalVariables(c("."))
