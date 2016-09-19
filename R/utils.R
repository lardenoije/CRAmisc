#' Pipe operator.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

# prevent R CMD check from creating a NOTE when utilizing "." in a pipeline
utils::globalVariables(c("."))
