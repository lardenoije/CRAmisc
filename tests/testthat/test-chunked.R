context("Chunked Functions")

# create in-memory SQLite database
dbcon <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
DBI::dbWriteTable(dbcon, "mtcars", mtcars)

# create and send a query
sql_query <- "SELECT * FROM mtcars where hp > 120"

# create a function to operate on chunks
# the function should accept a dataframe and return a dataframe
f <- function(df) {
  df %>%
    dplyr::mutate(hp_to_cyl = hp / cyl) %>%
    dplyr::arrange(hp_to_cyl) %>%
    dplyr::select(row_names, mpg, cyl, hp, wt, hp_to_cyl)
}

# tests
test_that("mtcars_new has the new column hp_to_cyl", {
  res <- DBI::dbSendQuery(dbcon, sql_query)
  mtcars_new <- chunked_pmap(res, f, 5)

  expect_named(mtcars_new,
               c("row_names", "mpg", "cyl", "hp", "wt", "hp_to_cyl"),
               ignore.order = TRUE,
               ignore.case = TRUE)
})

test_that("chunked_pmap throws an error when incorrect gc option passed", {
  res <- DBI::dbSendQuery(dbcon, sql_query)

  expect_error(chunked_pmap(res, f, 5, "x"))

  # because of the error, clear the result set
  DBI::dbClearResult(res)
})

test_that("garbage collection triggers without issue", {
  res <- DBI::dbSendQuery(dbcon, sql_query)
  mtcars_new <- chunked_pmap(res, f, 5, gc = "r")

  expect_named(mtcars_new,
               c("row_names", "mpg", "cyl", "hp", "wt", "hp_to_cyl"),
               ignore.order = TRUE,
               ignore.case = TRUE)
})

# cleanup
DBI::dbDisconnect(dbcon)
