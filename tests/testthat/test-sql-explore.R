context("SQL Server explore")

# get environment variables
# use environment variables to prevent being exposed publicly
# see also
#   https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html
#   http://blog.revolutionanalytics.com/2015/11/how-to-store-and-use-authentication-details-with-r.html
domain_user <- Sys.getenv("DOMAIN_USER")
domain_pw <- Sys.getenv("DOMAIN_PW")
domain_name <- Sys.getenv("DOMAIN_NAME")
test_server <- Sys.getenv("TEST_SERVER")
test_port <- Sys.getenv("TEST_PORT")
test_db <- Sys.getenv("TEST_DB")
test_table <- Sys.getenv("TEST_TABLE")
test_view <- Sys.getenv("TEST_VIEW")
test_col <- Sys.getenv("TEST_COL")

# connect
dbConnect_safely <- purrr::safely(DBI::dbConnect, otherwise = NA_character_)
src_sqlserver_safely <- purrr::safely(RSQLServer::src_sqlserver, otherwise = NA_character_)
con <- src_sqlserver_safely(server = test_server,
                            port = test_port,
                            database = test_db,
                            properties = list(
                              user = domain_user,
                              password = domain_pw,
                              domain = domain_name,
                              useNTLMv2 = TRUE
                            ))

# sample table / query
sql_query <- paste0("SELECT TOP 10 * FROM [", test_db, "].[dbo].[", test_view, "]")
if (is.null(con$error)) test_view_tibble <- dplyr::tbl(con$result, dplyr::sql(sql_query))

# tests
test_that("sql_list_schemas lists schemas", {
  skip_if_not(is.null(con$error),
              message = paste0("Unable to make connection to ", test_server))

  expect_match(sql_list_schemas(db_con = con$result, db_name = test_db),
               "dbo",
               all = FALSE,
               ignore.case = TRUE)
})

test_that("sql_list_tbls lists tables", {
  skip_if_not(is.null(con$error),
              message = paste0("Unable to make connection to ", test_server))

  expect_match(sql_list_tbls(db_con = con$result,
                             db_name = test_db,
                             db_schema = "dbo"),
               test_table,
               all = FALSE,
               ignore.case = TRUE)
})

test_that("sql_list_views lists views", {
  skip_if_not(is.null(con$error),
              message = paste0("Unable to make connection to ", test_server))

  expect_match(sql_list_views(db_con = con$result,
                              db_name = test_db,
                              db_schema = "dbo"),
               test_view,
               all = FALSE,
               ignore.case = TRUE)
})

test_that("sql_list_tvs lists tables and views", {
  skip_if_not(is.null(con$error),
              message = paste0("Unable to make connection to ", test_server))

  expect_match(sql_list_tvs(db_con = con$result,
                            db_name = test_db,
                            db_schema = "dbo"),
               test_view,
               all = FALSE,
               ignore.case = TRUE)
  expect_match(sql_list_tvs(db_con = con$result,
                            db_name = test_db,
                            db_schema = "dbo"),
               test_table,
               all = FALSE,
               ignore.case = TRUE)
})

test_that("sql_list_cols returns appropriate case", {
  skip_if_not(is.null(con$error),
              message = paste0("Unable to make connection to ", test_server))

  test_col_upper <- toupper(test_col)
  expect_equal(sql_list_cols(tbl = test_view_tibble,
                             cols = test_col_upper),
               test_col)
})

test_that("sql_list_cols can be used with a pipe", {
  skip_if_not(is.null(con$error),
              message = paste0("Unable to make connection to ", test_server))

  sql_list_cols_pipeline <- function(df, cols) {
    df %>%
      sql_list_cols(cols)
  }

  expect_equal(sql_list_cols_pipeline(test_view_tibble, test_col),
               test_col)
})