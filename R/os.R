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
