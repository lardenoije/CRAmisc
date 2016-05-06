###########
# require #
###########

# we need purrr, so make sure it is installed
#if (!require("purrr")) install.packages("purrr")
#if (!require("tibble")) install.packages("tibble")

# list of packages used
# if you don't specifically want to use source, then
#   just set the install_type to NA
#req_packages <- tibble::frame_data(
#  ~pkg, ~install_type,
#  "devtools", NA_character_,
#  "dplyr", NA_character_,
#  "lubridate", NA_character_,
#  "purrr", NA_character_,
#  "RSQLServer", "source"
#)

# function to install a package if it isn't already installed
#check_install <- function(pkg, install_type) {
#  if (!pkg %in% installed.packages()[,1]) {
#    if (is.na(install_type) | grepl("binary", install_type)) install.packages(pkg)
#    else install.packages(pkg, type = install_type)
#  } else cat(paste0(pkg, ' already installed\n'))
#}

# we call walk2 (walk over 2 lists) as we just want the side effects
# purrr::walk2(req_packages$pkg, req_packages$install_type, check_install)

# are we using packrat?
# check if the packrat directory exists
#using_packrat <- function() {
#  any(grepl("^packrat$", dir()))
#}

# if using packrat
# if (using_packrat()) packrat::snapshot()

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
