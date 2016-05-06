# transform a table rowwise in chunks
# helpful for when you need to be careful about memory consumption
#   such as when XMLs, JSON docs,  or blobs are embedded in database columns
# requires an expandingList -- see function above within data-structures

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

# may use for example / vignette
#dbcon <- DBI::dbConnect(drv = RSQLServer::SQLServer(),
#                        server = "SQL_TEST",
#                        file = '~/.sql.yaml')

# sql_query <- "SELECT * FROM [database].[schema].[table]"

# res <- DBI::dbSendQuery(dbcon, sql_query)

# perform the XML cleansing on the dataframe chunks returned
# this will be passed to rowwise_tr_chunks
# @importFrom dplyr "%>%"
#xml_processing <- function(df) {
#  df %>%
#    rowwise %>% # requires a dataframe
#    mutate(
#      my_name = get_xml_attr(
#        xml_col = XMLData,
#        xpath = "",
#        attr_name = "Name"
#    ),
#      my_result = get_xml_attr(
#        xml_col = XMLData,
#        xpath = "",
#        attr_name = "Result"
#    ))
#}

# process
# final_df <- rowwise_tr_chunks(res, 1000, xml_processing)
