#' Test how dataframes are the same / different.
#'
#' \code{df_diff} prints out how dataframes are the same and how they are
#' different.
#'
#' Tests include
#' \itemize{
#'   \item{Equal # of rows}
#'   \item{Equal # of columns}
#'   \item{Row names are the same}
#'   \item{Column names are the same}
#'   \item{Class(es) is / are the same}
#'   \item{Column types are the same - work in progress}
#'   \item{Values within each column are the same}
#' }
#' \code{df_diff} is unstable at the moment and should be considered a work in
#' progress.
#'
#' @param x First dataframe.
#' @param y Second dataframe.
#'
#' @return Test results and differences are printed to standard out.
#'
#' @examples
#' x <- tibble::frame_data(
#'  ~col1, ~col2,
#'  1, "a",
#'  2, "b",
#'  3, "c"
#' )
#'
#' y <- data.frame(col1 = 1:3, col2 = c("a", "b", "d"), col3 = c(10, 11, 12))
#'
#' df_diff(x, y)
#'
#' @export
df_diff <- function(x, y) {
  assertthat::assert_that(is.data.frame(x),
                          is.data.frame(y))

  # Equal # of rows ====
  nrow_equal <- identical(nrow(x), nrow(y))

  if(nrow_equal) {
    cat("The number of rows is equal\n")
  } else {
    cat("The number of columns is not equal\n")
    cat("  ", deparse(substitute(x)), "has", nrow(x), "rows\n")
    cat("  ", deparse(substitute(y)), "has", nrow(y), "rows\n")
  }

  # Equal # of columns ====
  ncol_equal <- identical(ncol(x), ncol(y))

  if(ncol_equal) {
    cat("The number of columns is equal\n")
  } else {
    cat("The number of columns is not equal\n")
    cat("  ", deparse(substitute(x)), "has", ncol(x), "columns\n")
    cat("  ", deparse(substitute(y)), "has", ncol(y), "columns\n")
  }

  # attributes
  attr_x <- attributes(x)
  attr_y <- attributes(y)

  # Row names are the same ====
  rownames_equal <- identical(attr_x$row.names, attr_y$row.names)

  if(rownames_equal) {
    cat("The row names are the same\n")
  } else {
    cat("The row names are not the same\n")
    cat("  ", deparse(substitute(x)), "has row names:", attr_x$row.names, "\n")
    cat("  ", deparse(substitute(y)), "has row names:", attr_y$row.names, "\n")
  }

  # Column names are the same
  colnames_equal <- identical(attr_x$names, attr_y$names)

  if(colnames_equal) {
    cat("The column names are the same\n")
  } else {
    cat("The column names are not the same\n")
    cat("  ", deparse(substitute(x)), "has column names:", attr_x$names, "\n")
    cat("  ", deparse(substitute(y)), "has column names:", attr_y$names, "\n")
  }

  # Column types are the same ====
  # work in progress

  # Class(es) is / are the same ====
  classes_equal <- identical(attr_x$class, attr_y$class)

  if(classes_equal) {
    cat("The class(es) is / are the same\n")
  } else {
    cat("The class(es) is / are not the same\n")
    cat("  ", deparse(substitute(x)), "has class(es):", attr_x$class, "\n")
    cat("  ", deparse(substitute(y)), "has class(es):", attr_y$class, "\n")
  }

  # Values within each column are the same ====
  if(!nrow_equal) {
    cat("The number of rows is not equal;",
        "testing of values within each column will not occur\n")
  } else {
    if(!ncol_equal) {
      cat("The number of columns is not equal;",
          "testing will occur on columns that are named the same\n")
    } else {
      cat("The number of columns is equal;",
          "testing will occur on all columns\n")
    }
    uniq_colnames <- unique(attr_x$names, attr_y$names)
    if(length(uniq_colnames) != 0L) {
      for(i in seq_along(uniq_colnames)) {
        cat("  column:", uniq_colnames[i], "\n",
            "  all equal:",
            all.equal(x[,uniq_colnames[i]], y[,uniq_colnames[i]]), "\n")
      }
    } else {
      cat("  There are not any columns names that are the same;",
          "no comparison will be made")
    }
  }
}