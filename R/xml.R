# create safe versions
# different form of error handling
#safe_xml_find_one <- purrr::safely(xml2::xml_find_one, otherwise = NA_character_)
#safe_xml_find_all <- purrr::safely(xml2::xml_find_all, otherwise = NA_character_)
#safe_xml_attr <- purrr::safely(xml2::xml_attr, otherwise = NA_character_)
#safe_xml_text <- purrr::safely(xml2::xml_text, otherwise = NA_character_)

#get_xml_attr <- function(xml_col, xpath, attr_name) {
  # using the xpath, find the attribute
#  x <- xml2::read_xml(xml_col)
#  xml_col_node <- safe_xml_find_one(x, xpath, ns = xml2::xml_ns(x))
#  xml_col_attr <- safe_xml_attr(xml_col_node$result, attr_name)
  #return(xml_col_attr$result)
#}

# this is the core function that parses the XML
# it will be wrapped in another function before operating on the df
#   the underscore at the end of its name suggests that is a "private" function
#     and should be used via a wrapper (or via a "public" function)
# parameters:
#   xml = xml document
#     if coming from a dataframe, the XML column will be passed in
#   xpath = xpath to use for parsing
#   extract_type = type of XML extraction - "text" or "attr"
#     "text" ==> <var1>10</var1>
#     "attr" ==> <var1 value1="10"></var1>
#   extract_value = if extract_type = "attr" then what is the attribute that
#     contains the value to extract
#     extract_value = "value1" ==> <var1 value1="10"></var1>
#   ret_var_name = what should the value being extracted be named
#   dots (...) = are required so that columns other than the XML column are
#     passed through the mapping functions
#xml_get_value_ <- function(xml,
#                           xpath,
#                           extract_type,
                           #extract_value,
                           #ret_var_name,
                           #...) {

#  if (!extract_type %in% c("text", "attr")) {
#    stop("var_name must be either \"text\" or \"attr\"")
#  }

  # read the entire xml into memory
  #xml_doc <- read_xml(xml)

  # find the appropriate node using xpath
#  xml_node <- safe_xml_find_one(xml_doc, xpath, ns = xml_ns(xml_doc))

  # extract the actual value
#  if (extract_type == "attr") {
  #  xpath_result <- safe_xml_attr(xml_node$result, extract_value)$result
  #} else {
  #  xpath_result <- safe_xml_text(xml_node$result)$result
  #}

  # assign a name to the result
  #names(xpath_result) <- ret_var_name
#
#  return(xpath_result)
#}