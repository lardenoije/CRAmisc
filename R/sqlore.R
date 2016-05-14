#' Generate SQL query.
#'
#' \code{sql_list_sql_} returns an SQL query as a string based on the parameters
#' passed to it.
#'
#' \code{sql_list_sql_} is used along with the function factory
#' \code{\link{sql_list_}} to create functions that return properties of a
#' Microsoft SQL Server database.
#'
#' @section Warning: The intent is not to use this function directly.  Instead
#'   use one of the functions
#'   \itemize{
#'     \item \code{\link{sql_list_schemas}}
#'     \item \code{\link{sql_list_tbls}}
#'     \item \code{\link{sql_list_views}}
#'     \item \code{\link{sql_list_tvs}}
#'   }
#'
#' @param db_con Database server connection object.
#' @param db_name Database name.
#' @param db_schema Database schema.
#' @param obj String used with \code{switch} to select appropriate SQL query.
#'   Choices include
#'   \itemize{
#'     \item \code{"schemas"}
#'     \item \code{"tables"}
#'     \item \code{"views"}
#'     \item \code{"tvs"}
#'   }
#'
#' @return An SQL query as a string.
#'
sql_list_sql_ <- function(db_con, db_name, db_schema, obj) {
  # switch is weird in R
  sql_statement <-
    switch(obj,
           "schemas"={
             paste0("SELECT DISTINCT(b.name)
                    FROM [", db_name, "].[sys].[objects] as a
                    LEFT JOIN [", db_name, "].[sys].[schemas] as b
                    ON a.schema_id = b.schema_id
                    WHERE a.type in ('U','V')")
           },
           "tables"={
             paste0("SELECT name
                    FROM [", db_name, "].[sys].[objects]
                    WHERE type in ('U')
                    and schema_id in (SELECT schema_id
                    FROM [", db_name, "].[sys].[schemas]
                    WHERE name = '", db_schema, "')")
           },
           "views"={
             paste0("SELECT name
                    FROM [", db_name, "].[sys].[objects]
                    WHERE type in ('V')
                    and schema_id in (SELECT schema_id
                    FROM [", db_name, "].[sys].[schemas]
                    WHERE name = '", db_schema, "')")
           },
           "tvs"={
             paste0("SELECT name
                    FROM [", db_name, "].[sys].[objects]
                    WHERE type in ('U','V')
                    and schema_id in (SELECT schema_id
                    FROM [", db_name, "].[sys].[schemas]
                    WHERE name = '", db_schema, "')")
           }
    )
}


#' Create sql_list_xyz functions.
#'
#' \code{sql_list_} is a function factory to create functions that return
#' properties of a Microsoft SQL Server database.
#'
#' \code{sql_list_} is used along with \code{\link{sql_list_sql_}} to create
#' functions.
#'
#' @section Warning: The intent is not to use this function directly.  Instead
#'   use one of the functions
#'   \itemize{
#'     \item \code{\link{sql_list_schemas}}
#'     \item \code{\link{sql_list_tbls}}
#'     \item \code{\link{sql_list_views}}
#'     \item \code{\link{sql_list_tvs}}
#'   }
#'
#' @param obj String used with \code{switch} in the function
#'   \code{\link{sql_list_sql_}} to select appropriate SQL query. Choices include
#'   \itemize{
#'     \item \code{"schemas"}
#'     \item \code{"tables"}
#'     \item \code{"views"}
#'     \item \code{"tvs"}
#'   }
#'
#' @return A function that can be assigned a custom variable name.
#'
#' @seealso
#'   \href{http://adv-r.had.co.nz/Functional-programming.html#closures}{Closures}
#'
sql_list_ <- function(obj) {
  if (obj == "schemas") {
    function(db_con, db_name) {
      sql_statement <- sql_list_sql_(db_con = db_con,
                                     db_name = db_name,
                                     db_schema = NULL,
                                     obj = obj)
      # assumes that the column name returned is always 'name'
      sort(as.data.frame(dplyr::tbl(db_con, dplyr::sql(sql_statement)))$name)
    }
  } else {
    function(db_con, db_name, db_schema) {
      sql_statement <- sql_list_sql_(db_con = db_con,
                                     db_name = db_name,
                                     db_schema = db_schema,
                                     obj = obj)
    # assumes that the column name returned is always 'name'
    sort(as.data.frame(dplyr::tbl(db_con, dplyr::sql(sql_statement)))$name)
    }
  }
}

#' List schemas.
#'
#' List all schemas in a Microsoft SQL Server database.
#'
#' @inheritParams sql_list_sql_
#'
#' @return A character vector of all schemas.
#'
#' @examples
#' \dontrun{
#' con <- RSQLServer::src_sqlserver(server = "dbserver_dbname", file = "~/.sql.yaml")
#' dbserver_schemas <- sql_list_schemas(db_con = con,
#'                                      db_name = "dbname")
#' }
#'
#' @family sql_lists
#'
#' @export
sql_list_schemas <- sql_list_("schemas")


#' List tables.
#'
#' List all tables in a Microsoft SQL Server database.
#'
#' @section Warning:
#' Will not return views.  If tables and views are desired, use
#' \code{\link{sql_list_tvs}}.
#'
#' @inheritParams sql_list_sql_
#'
#' @return A character vector of all tables.
#'
#' @examples
#' \dontrun{
#' con <- RSQLServer::src_sqlserver(server = "dbserver_dbname", file = "~/.sql.yaml")
#' dbserver_tables <- sql_list_tbls(db_con = con,
#'                                  db_name = "dbname",
#'                                  db_schema = "dbschema")
#' }
#'
#' @family sql_lists
#'
#' @export
sql_list_tbls <- sql_list_("tables")


#' List views.
#'
#' List all views in a Microsoft SQL Server database.
#'
#' @section Warning:
#' Will not return tables.  If tables and views are desired, use
#' \code{\link{sql_list_tvs}}.
#'
#' @inheritParams sql_list_sql_
#'
#' @return A character vector of all views.
#'
#' @examples
#' \dontrun{
#' con <- RSQLServer::src_sqlserver(server = "dbserver_dbname", file = "~/.sql.yaml")
#' dbserver_views <- sql_list_views(db_con = con,
#'                                  db_name = "dbname",
#'                                  db_schema = "dbschema")
#' }
#'
#' @family sql_lists
#'
#' @export
sql_list_views <- sql_list_("views")


#' List tables and views.
#'
#' List all tables and views in a Microsoft SQL Server database.
#'
#' @inheritParams sql_list_sql_
#'
#' @return A character vector of all tables and views.
#'
#' @examples
#' \dontrun{
#' con <- RSQLServer::src_sqlserver(server = "dbserver_dbname", file = "~/.sql.yaml")
#' dbserver_tvs <- sql_list_tvs(db_con = con,
#'                              db_name = "dbname",
#'                              db_schema = "dbschema")
#' }
#'
#' @family sql_lists
#'
#' @export
sql_list_tvs <- sql_list_("tvs")


#' Get exact names of columns from SQL Server.
#'
#' Because Microsoft SQL Server column names can be case sensitive, get the exact column names
#' as SQL Server stores them.
#'
#' @param cols character vector of column names
#' @param tbl table object (tibble)
#'
#' @return A character vector of column names with the appropriate case.
#'
#' @examples
#' \dontrun{
#' # connection and tibble objects
#' con <- RSQLServer::src_sqlserver(server = "dbserver_dbname", file = "~/.sql.yaml")
#' my_tibble <- dplyr::tbl(con, dplyr::sql("SELECT * FROM [dbname].[dbschema].[dbtable]"))
#'
#' # column names
#' my_cols <- c("mixedCasecol", "anothermixedCASECol")
#'
#' # use
#' my_tibble %>%
#'   sql_list_cols(my_cols)
#' }
#'
#' @family sql_lists
#'
#' @export
sql_list_cols <- function(tbl, cols) {
  purrr::map_chr(
    cols,
    function(col) dplyr::tbl_vars(tbl)[match(tolower(col),
                                             tolower(dplyr::tbl_vars(tbl)))]
  )
}