#' Get XML Attribute
#'
#' Return an XML node attribute if provided the document, XPath, and attribute
#' name.
#'
#' Serves as a convenience function.  Utilizes \link[xml2]{xml_find_first}
#' and \link[xml2]{xml_attr} and will return \code{NA_character_} if the XML
#' attribute cannot be located or an error occurs.
#'
#' Because the function uses \link[xml2]{xml_find_first}, only the first match
#' is returned.
#'
#' @param x XML document: a literal XML document, a URL, or a string
#' @param xpath A string containing a xpath (1.0) expression.
#' @param extract_value A string containing the attribute name to extract.
#'
#' @return XML node attribute as a string or \code{NA_character_} if an error
#' occurs.
#'
#' @examples
#' xml_doc <- "
#' <root_tag>
#'   <another_tag>
#'     <var_text>10</var_text>
#'     <var_attr value=\"10\"></var_attr>
#'   </another_tag>
#' </root_tag>
#' "
#'
#' ten <- get_xml_attr(xml_doc,
#'                     "//root_tag/another_tag/var_attr",
#'                     "value")
#' identical(ten, "10")
#'
#' @export
get_xml_attr <- function(x, xpath, extract_value) {
  xml_doc <- xml2::read_xml(x)
  xml_node <- xml2::xml_find_first(xml_doc, xpath, ns = xml2::xml_ns(xml_doc))
  xml_attr <- xml2::xml_attr(xml_node, extract_value)
  return(xml_attr)
}


#' Get XML Text
#'
#' Return an XML text value if provided the document and Xpath.
#'
#' Serves as a convenience function.  Utilizes \link[xml2]{xml_find_first}
#' and \link[xml2]{xml_text} and will return \code{NA_character_} if the XML
#' text cannot be located or an error occurs.
#'
#' Because the function uses \link[xml2]{xml_find_first}, only the first match
#' is returned.
#'
#' @param x XML document: a literal XML document, a URL, or a string.
#' @param xpath A string containing a xpath (1.0) expression.
#'
#' @return XML text as a string or \code{NA_character_} if an error occurs.
#'
#' @examples
#' xml_doc <- "
#' <root_tag>
#'   <another_tag>
#'     <var_text>10</var_text>
#'     <var_attr value=\"10\"></var_attr>
#'   </another_tag>
#' </root_tag>
#' "
#'
#' ten <- get_xml_text(xml_doc,
#'                     "//root_tag/another_tag/var_text")
#' identical(ten, "10")
#'
#' @export
get_xml_text <- function(x, xpath) {
  xml_doc <- xml2::read_xml(x)
  xml_node <- xml2::xml_find_first(xml_doc, xpath, ns = xml2::xml_ns(xml_doc))
  xml_text <- xml2::xml_text(xml_node)
  return(xml_text)
}

#' XML Extract
#'
#' Extract XML text or an XML attribute via XPath.
#'
#' May be used as a UDF that is part of a \code{dplyr} pipeline.  The most
#' simple use is to include \code{xml_extract} as part of a \code{dplyr::mutate}
#' function.  For details see \code{vignette("chunked-invoke-rows-xml")}.
#'
#' A more complex use would be to use as a UDF to parse an arbitrary number of
#' text and attribute values from an XML document.  This can be accomplished
#' utilizing a dataframe holding parameter values, \code{purrr::pmap}, and
#' \code{purrr::invoke_rows}.
#'
#' Because \link[xml2]{xml_find_first} is the function utilized in
#' \code{xml_extract}, errors are consumed.  This is helpful when iterating over
#' a set of XML documents where the schemas are inconsistent.
#'
#' Suggested resources for XPath are
#' \itemize{
#'   \item \url{https://developer.mozilla.org/en-US/docs/Web/XPath/Functions}
#'   \item \url{http://www.w3schools.com/xsl/xpath_syntax.asp}
#' }
#'
#' @param x XML document: a literal XML document, a URL, or a string.
#' @param xpath A string containing a xpath (1.0) expression.
#' @param extract_type The string "text" or "attr" selecting the type of
#' extraction.
#' @param extract_value The attribute value to be extracted.  This only needs to
#' be set if \code{extract_type = "attr"}.
#' @param ret_var_name The variable name of the extracted value.
#' \code{xml_extract} may be used as part of an assignment statement.  If this
#' is the case, then the parameter \code{ret_var_name} should remain
#' \code{NULL}.  But if \code{xml_extract} is to be used as part of a functional
#' pipeline then it may be necessary to name the returned value.
#'
#' @return The extracted text or attribute value from an XML tag.
#'
#' @seealso
#' See \code{vignette("chunked-invoke-rows")} for usage.
#'
#'
#' @export
xml_extract <- function(x,
                        xpath,
                        extract_type,
                        extract_value = NULL,
                        ret_var_name = NULL) {

  if (!extract_type %in% c("text", "attr")) {
    stop("extract_type must be either \"text\" or \"attr\"")
  }

  if (is.null(extract_value) && extract_type == "attr") {
    stop("extract_value cannot be NULL if extract_type = \"attr\"")
  }

  # read the entire xml into memory
  xml_doc <- xml2::read_xml(x)

  # find the appropriate node using xpath
  xml_node <- xml2::xml_find_first(xml_doc, xpath, ns = xml2::xml_ns(xml_doc))

  # extract the actual value
  if (extract_type == "attr") {
    xpath_result <- xml2::xml_attr(xml_node, extract_value)
  } else {
    xpath_result <- xml2::xml_text(xml_node)
  }

  # assign a name to the result
  # useful if using with pmap and invoke_rows
  if(!is.null(ret_var_name)) names(xpath_result) <- ret_var_name

  return(xpath_result)
}
