#' Convert dataframe column data types.
#'
#' Given a configuration dataframe with column names and the required types,
#' convert the column data types in a separate dataframe.
#'
#' Column metadata may be stored separately from the code needed to perform the
#' conversion.  As an example, a dataframe with 2,000 columns could have its
#' metadata stored in a separate \code{csv} or \code{xlsx} file.  The metadata
#' file would be read in as a dataframe and \code{convert_cols} would update the
#' data types en masse.
#'
#' \code{convert_cols} is trading the need to maintain a separate metadata file
#' for concise code that performs the data type conversion.  The alternative is
#' to have verbose code that details each data type conversion to take place.
#'
#' \code{convert_cols} is a closure that contains two parts.
#' \itemize{
#'   \item An inner function, \code{convert_}, that converts a set of columns
#'   that are to be converted to the same type using \link[purrr]{dmap_at}.
#'   \item The enclosing environment that contains the dataframe that will be
#'   updated and contains a call to \link[purrr]{invoke_rows} to drive
#'   iteration.
#' }
#'
#' Finally, the function is missing cases to convert to the following types.
#' The types includes should cover the most simple use cases.
#' \itemize{
#'   \item list
#'   \item factor
#'   \item raw
#'   \item complex
#' }
#'
#' @param df A dataframe that will have its column data types converted.
#' @param types_df A dataframe containing the column to convert and the data
#' type to convert to.  The expectation is that \code{types_df} has columns
#' named \code{col_name} and \code{col_type}.  If \code{types_df} has columns
#' named differently, this is easy to convert using \link[dplyr]{rename}.
#'
#' @return A copy of the original dataframe, df, with the column types
#' converted.
#'
#' @examples
#' ## dataframe to convert
#' id <- c(1L, 2L, 3L)
#' date_var <- c("2016-01-02", "2015-12-24", "2016-05-05")
#' datetime_var <- c("2016-01-02T11:11:04",
#'                   "2015-12-24T08:11:41",
#'                   "2016-05-05T05:05:49")
#' double_var1 <- c("46.41", "118.11", "84.68")
#' double_var2 <- c("68.48", "-248.99", "194")
#' numeric_var <- c("78.61", "593.1", "123")
#' logical_var <- c("true", "true", "false")
#' character_var <- c(49, 88, 104)
#' integer_var <- c("77", "84", "4949")
#'
#' test_df <- data.frame(id,
#'                       date_var,
#'                       datetime_var,
#'                       double_var1,
#'                       double_var2,
#'                       numeric_var,
#'                       logical_var,
#'                       character_var,
#'                       integer_var)
#'
#' ## print the structure
#' str(test_df)
#'
#' ## create a dataframe with columns named col_name and col_type (if you name
#' ##   them something else you can always use dplyr::rename) with the column
#' ##   name and the type you would like it converted to
#' types_df <- tibble::tribble(
#'   ~col_name, ~col_type,
#'   "date_var", "date",
#'   "datetime_var", "datetime",
#'   "double_var1", "double",
#'   "double_var2", "double",
#'   "numeric_var", "numeric",
#'   "logical_var", "logical",
#'   "character_var", "character",
#'   "integer_var", "integer"
#' )
#'
#' ## convert the columns
#' test_df_converted <- convert_cols(test_df, types_df)
#'
#' @export
convert_cols <- function(df, types_df) {
  # trailing underscore represents a "private" function
  convert_ <- function(col_type, col_names, ...) {
    # swallow dots when using as part of invoke_rows
    # used in case types_df has other columns contained within
    dots <- list(...)

    # type check as well as setting the function to perform conversion
    if (col_type %in% c("character",
                        "date",
                        "datetime",
                        "double",
                        "integer",
                        "logical")) {
      conv_func <- switch(col_type,
                          "character" = as.character,
                          "date" = lubridate::ymd,
                          "datetime" = lubridate::ymd_hms,
                          "double" = as.double,
                          "integer" = as.integer,
                          "logical" = as.logical)
    } else if (col_type  %in% ("numeric")) {
      message("For col_type = 'numeric' utilizing type double")
      conv_func <- as.double
    } else {
      stop("col_type must be one of:\n    character, date, datetime, double, integer, or logical")
    }

    # col_names is a list, convert to a character vector
    col_names_chr <- col_names %>% purrr::flatten_chr()

    # name check
    # if not all columns to convert are in df_converted
    if (!all(tibble::has_name(df_converted, col_names_chr))) {
      # discard those columns that are not in df_converted
      col_names_chr_not <- purrr::discard(col_names_chr,
                                          tibble::has_name(df_converted,
                                                           col_names_chr))
      # keep just those columns that are in df_converted
      col_names_chr <- purrr::keep(col_names_chr,
                                   tibble::has_name(df_converted,
                                                    col_names_chr))
      message(paste0("Ignoring the following columns:\n  ",
                     paste(col_names_chr_not, collapse = "\n  "))
      )
    }
    # perform the conversion
    # because using a closure, able to use <<- to update df_converted
    df_converted <<- purrr::dmap_at(.d = df_converted,
                                    .at = col_names_chr,
                                    .f = conv_func)
  }

  # the dataframe to update
  df_converted <- df

  # utilize tidyr::nest so that conversions can happen in groups
  #   e.g. all character conversions happen at the same time in a single
  #     purrr::dmap_at call rather than one at a time
  types_df_nest <- types_df %>%
    # make sure that all columns are character and not factors
    dplyr::mutate_if(is.factor, as.character) %>%
    tidyr::nest_("col_name", key_col = "col_names")

  # iterate over the types dataframe, which tells us how to convert columns
  #   in df
  # if there are columns within types_df other than col_name and col_type,
  #   they are swallowed by the fact that convert_ takes dots (...)
  purrr::invoke_rows(.d = types_df_nest,
                     .f = convert_)

  # return the new dataframe
  df_converted
}


#' Convert a readr column specification.
#'
#' Given a \link[readr]{cols} specification, update the specification in place
#' by explicitly declaring the appropriate type in a dataframe.
#'
#' This is of value when importing an extremely wide dataframe with many
#' columns.  It may not make sense to explicitly declare the type of every
#' column by hand.  Nor should one have to copy/paste an exisiting column
#' specification into a text editor in order to update the specification.
#'
#' A final use case is when problems occur and a \link[readr]{problems} tibble
#' is created.  The tibble can be the starting point to decide upon the columns
#' that require updating.
#'
#' \code{col_spec_update} is a closure that contains two parts.
#' \itemize{
#'   \item An inner function, \code{update_}, that updates the specification by
#'   setting the \code{class} attribute for the given column.
#'   \item The enclosing environment that contains the specification to be
#'   updated and contains a call to \link[purrr]{invoke_rows} to drive
#'   iteration.
#' }
#'
#' @param col_spec A \link[readr]{cols} specification.  Most commonly produced
#' as part of a call to \link[readr]{read_csv}.
#' @param col_spec_df A dataframe containing the column name and type to update
#' within the specification.  The expectation is that \code{col_spec_df} has
#' columns named \code{col_name} and \code{col_type}.  If \code{col_spec_df}
#' has columns named differently, this is easy to convert using
#' \link[dplyr]{rename}.
#'
#' @return A copy of the original specification with updated column types.
#'
#' @examples
#' test_df <- readr::read_csv("a,b,c\n1,2,3\n4,5,6")
#' test_spec <- readr::spec(test_df)
#'
#' ## returns
#' ##   a = col_integer()
#' ##   b = col_integer()
#' ##   c = col_integer()
#'
#' ## update columns a and b to be doubles instead of integers
#'
#' col_spec_df <- tibble::tribble(
#'   ~col_name, ~col_type,
#'   "a", "double",
#'   "b", "double"
#' )
#'
#' ## update the specification
#' test_spec_updated <- col_spec_update(test_spec, col_spec_df)
#'
#' ## re-read with new column spec
#' test_updated <- readr::read_csv("a,b,c\n1,2,3\n4,5,6",
#'                                 col_types = test_spec_updated)
#' @export
col_spec_update <- function(col_spec, col_spec_df) {

  # trailing underscore represents a "private" function
  update_ <- function(col_name, col_type, ...) {
    # swallow dots when using as part of invoke_rows
    # used in case col_spec_df has other columns contained within
    dots <- list(...)

    # type check as well as create the collector type to utilize to set the
    #   class attribute
    if (col_type %in% c("character",
                        "date",
                        "datetime",
                        "double",
                        "euro_double",
                        "integer",
                        "logical",
                        "number",
                        "time",
                        "skip",
                        "guess")) {
      col_type_full <- c(paste0("collector_", col_type), "collector")
    } else {
      stop("col_type must be one of:\n    character, date, datetime, double, euro_double, integer, logical, number, time, skip, or guess")
    }

    # check that col_name is part of the specification
    if (col_name %in% names(col_spec_updated$cols)) {
      attr(col_spec_updated$cols[col_name][[1]], "class") <<- col_type_full
    } else {
      message(paste0("Ignoring as the following column name ",
                     "is not part of the specification:  ", col_name))
    }
  }

  # create a copy of col_spec
  col_spec_updated <- col_spec

  # iterate over the col_spec_df dataframe, which tells us how to update the
  #   column specification
  purrr::invoke_rows(.d = col_spec_df,
                     .f = update_)

  # return the updated spec
  return(col_spec_updated)
}
