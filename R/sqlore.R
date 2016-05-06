#' Generate SQL query.
#'
#' \code{db_list_sql_} returns an SQL query as a string based on the parameters
#' passed to it.
#'
#' \code{db_list_sql_} is used along with the function factory
#' \code{\link{db_list_}} to create functions that return properties of a
#' Microsoft SQL Server database.
#'
#' @section Warning: The intent is not to use this function directly.  Instead
#'   use one of the functions
#'   \itemize{
#'     \item \code{\link{db_list_schemas}}
#'     \item \code{\link{db_list_tables}}
#'     \item \code{\link{db_list_views}}
#'     \item \code{\link{db_list_tvs}}
#'   }
#'
#' @param db_con Database server connection object.
#' @param db_name Database name.
#' @param db_schema Database schema.
#' @param obj String used with \code{switch} to select appropriate SQL query.
#'   Choices include \code{"schemas"}, \code{"tables"}, \code{"views"}, and
#'   \code{"tvs"}.
#'
#' @return An SQL query as a string.
db_list_sql_ <- function(db_con, db_name, db_schema, obj) {
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


#' Create db_list_xyz functions.
#'
#' \code{db_list_} is a function factory to create functions that returns
#' properties of a Microsoft SQL Server database.
#'
#' \code{db_list_} is used along with \code{\link{db_list_sql_}} to create
#' functions.
#'
#' @section Warning: The intent is not to use this function directly.  Instead
#'   use one of the functions
#'   \itemize{
#'     \item \code{\link{db_list_schemas}}
#'     \item \code{\link{db_list_tables}}
#'     \item \code{\link{db_list_views}}
#'     \item \code{\link{db_list_tvs}}
#'   }
#'
#' @param obj String used with \code{switch} in the function
#'   \code{\link{db_list_sql_}} to select appropriate SQL query. Choices include
#'   \code{"schemas"}, \code{"tables"}, \code{"views"}, and \code{"tvs"}.
#'
#' @return A function that can be assigned a custom variable name.
#'
#' @seealso
#'   \href{http://adv-r.had.co.nz/Functional-programming.html#closures}{Closures}
db_list_ <- function(obj) {
  function(db_con, db_name, db_schema) {
    sql_statement <- db_list_sql_(db_con = db_con,
                                  db_name = db_name,
                                  db_schema = db_schema,
                                  obj = obj)
    # assumes that the column name returned is always 'name'
    sort(as.data.frame(dplyr::tbl(db_con, dplyr::sql(sql_statement)))$name)
  }
}

#' List schemas.
#'
#' List all schemas in a Microsoft SQL Server database.
#'
#' @inheritParams db_list_sql_
#'
#' @return A character vector of all schemas.
#'
#' @examples
#' \dontrun{
#' con <- RSQLServer::src_sqlserver(server = "dbserver_dbname", file = "~/.sql.yaml")
#' dbserver_schemas <- db_list_schemas(db_con = con)
#' }
#' @export
db_list_schemas <- db_list_("schemas")


#' List tables.
#'
#' List all tables in a Microsoft SQL Server database.
#'
#' @section Warning:
#' Will not return views.  If tables and views are desired, use
#' \code{\link{db_list_tvs}}.
#'
#' @inheritParams db_list_sql_
#'
#' @return A character vector of all tables.
#'
#' @examples
#' \dontrun{
#' con <- RSQLServer::src_sqlserver(server = "dbserver_dbname", file = "~/.sql.yaml")
#' dbserver_tables <- db_list_tables(db_con = con, db_name = "dbname")
#' }
#' @export
db_list_tables <- db_list_("tables")


#' List views.
#'
#' List all views in a Microsoft SQL Server database.
#'
#' @section Warning:
#' Will not return tables.  If tables and views are desired, use
#' \code{\link{db_list_tvs}}.
#'
#' @inheritParams db_list_sql_
#'
#' @return A character vector of all views.
#'
#' @examples
#' \dontrun{
#' con <- RSQLServer::src_sqlserver(server = "dbserver_dbname", file = "~/.sql.yaml")
#' dbserver_views <- db_list_views(db_con = con,
#'                                 db_name = "dbname",
#'                                 db_schema = "dbschema")
#' }
#'
#' @export
db_list_views <- db_list_("views")


#' List tables and views.
#'
#' List all tables and views in a Microsoft SQL Server database.
#'
#' @inheritParams db_list_sql_
#'
#' @return A character vector of all tables and views.
#'
#' @examples
#' \dontrun{
#' con <- RSQLServer::src_sqlserver(server = "dbserver_dbname", file = "~/.sql.yaml")
#' dbserver_tvs <- db_list_tvs(db_con = con,
#'                             db_name = "dbname",
#'                             db_schema = "dbschema")
#' }
#'
#' @export
db_list_tvs <- db_list_("tvs")


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
#' my_tbl <- dplyr::tbl(con, sql("SELECT * FROM [dbname].[dbschema].[mytable]"))
#'
#' # get column names
#' my_cols <- sql_col_names(c("mixedCasecol", "anothermixedCol"))
#
#' # use
#' my_tbl %>% select(my_cols)
#' }
#'
#' @export
sql_col_names <- function(cols, tbl) {
  purrr::map_chr(
    cols,
    function(col) dplyr::tbl_vars(tbl)[match(tolower(col),
                                             tolower(dplyr::tbl_vars(tbl)))]
  )
}