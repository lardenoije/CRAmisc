###########
# require #
###########

# we need purrr, so make sure it is installed
if (!require("purrr")) install.packages("purrr")
if (!require("tibble")) install.packages("tibble")

# list of packages used
# if you don't specifically want to use source, then
#   just set the install_type to NA
req_packages <- tibble::frame_data(
  ~pkg, ~install_type,
  "devtools", NA_character_,
  "dplyr", NA_character_,
  "lubridate", NA_character_,
  "purrr", NA_character_,
  "RSQLServer", "source"
)

# function to install a package if it isn't already installed
check_install <- function(pkg, install_type) {
  if (!pkg %in% installed.packages()[,1]) {
    if (is.na(install_type) | grepl("binary", install_type)) install.packages(pkg)
    else install.packages(pkg, type = install_type)
  } else cat(paste0(pkg, ' already installed\n'))
}

# we call walk2 (walk over 2 lists) as we just want the side effects
# purrr::walk2(req_packages$pkg, req_packages$install_type, check_install)

# are we using packrat?
# check if the packrat directory exists
using_packrat <- function() {
  any(grepl("^packrat$", dir()))
}

# if using packrat
# if (using_packrat()) packrat::snapshot()

# install data.table development version >= 1.9.7
#   in order to use fwrite
# https://github.com/Rdatatable/data.table/wiki/Installation
# install.packages("data.table", type = "source",
#                 repos ="https://Rdatatable.github.io/data.table")

###########
# utility #
###########

# ARGH - these really need to go into their own packages

# fast expanding list
# http://stackoverflow.com/a/32870310
expandingList <- function(capacity = 10) {
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

# transform a table rowwise in chunks
# helpful for when you need to be careful about memory consumption
#   as is the case when XMLs (or blobs) are embedded in database columns
# requires an expandingList -- see function above

# res ==> DBI::dbSendQuery result
# n ==> number of chunks / rows to pull back at a time
# f ==> the function that will be used to process the returned dataframe
#       the function is assumed to be constructed using a dplyr pipeline
#         where the only argument passed to the function is the dataframe
rowwise_tr_chunks <- function(res, n, f) {
  # utilize dbHasCompleted and dbFetch to iteratively
  #   fetch n rows and append to an expandingList
  # an expandingList is a special type of list that allows
  #   for dynamic expansion without the overhead of copies that occur
  df_list <- expandingList()

  while (!DBI::dbHasCompleted(res)) {
    df_chunk <- DBI::dbFetch(res, n = n) # dataframe

    df_chunk <- f(df_chunk)

    df_list$add(df_chunk)
  }
  # convert to an actual list as opposed to an expandingList
  df_list <- df_list$as.list()

  # final step is to bind into a single dataframe
  dplyr::bind_rows(df_list)
}

# closure
# http://adv-r.had.co.nz/Functional-programming.html#closures
is_os <- function(os_string) {
  function() {
    grepl(pattern = os_string, sessionInfo()$running, ignore.case = TRUE)
  }
}

# create functions
is_osx <- is_os("os x")
is_linux <- is_os("linux")
is_windows <- is_os("windows")
