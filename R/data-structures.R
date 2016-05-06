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
