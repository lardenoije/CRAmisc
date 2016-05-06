# create safe versions
# different form of error handling
safe_xml_find_one <- purrr::safely(xml2::xml_find_one, otherwise = NA_character_)
safe_xml_find_all <- purrr::safely(xml2::xml_find_all, otherwise = NA_character_)
safe_xml_attr <- purrr::safely(xml2::xml_attr, otherwise = NA_character_)

get_xml_attr <- function(xml_col, xpath, attr_name) {
  # using the xpath, find the attribute
  x <- xml2::read_xml(xml_col)
  xml_col_node <- safe_xml_find_one(x, xpath, ns = xml2::xml_ns(x))
  xml_col_attr <- safe_xml_attr(xml_col_node$result, attr_name)
  return(xml_col_attr$result)
}