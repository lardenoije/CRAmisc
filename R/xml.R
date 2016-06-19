#' Safe \code{xml2} find functions
#'
#' Safe version of \code{xml2} find functions
#'
#' The safe versions are helpful when iterating over a set of XML documents
#' and the schemas are inconsistent.  Rather than throwing errors, the safe find
#' function will return \code{NA_character_} when unable to find a node.
#'
#' Suggested resources for XPath are
#' \itemize{
#'   \item \url{https://developer.mozilla.org/en-US/docs/Web/XPath/Functions}
#'   \item \url{http://www.w3schools.com/xsl/xpath_syntax.asp}
#' }
#'
#' @param x A document, node, or node set.
#' @param xpath A string containing a xpath (1.0) expression.
#' @param ns Optionally, a named vector giving prefix-url pairs, as produced
#' by \code{\link[xml2]{xml_ns}}. If provided, all names will be explicitly
#' qualified with the ns prefix, i.e. if the element \code{bar} is defined
#' in namespace \code{foo}, it will be called \code{foo:bar}. (And
#' similarly for atttributes). Default namespaces must be given an explicit
#' name.
#'
#' @return \code{safe_xml_find_all} and \code{safe_xml_find_one} will either
#' return a nodeset (or node) or return \code{NA_character_} if there is not
#' a match.
#'
#' @seealso
#' \itemize{
#'   \item \link[xml2]{xml_find_all}
#'   \item \link[xml2]{xml_find_one}
#' }
#'
#' @seealso
#'
#' @examples
#' var_text_node <- safe_xml_find_one(xml2::read_xml(xml_doc),
#'                                    xpath = "//root_tag/another_tag/var_text")
#'
#' # returns a nodeset without an error
#' var_text_node$result
#' is.null(var_text_node$error)
#'
#' var_text2_node <- safe_xml_find_one(xml2::read_xml(xml_doc),
#'                                     xpath = "//root_tag/another_tag/var_text2")
#'
#' # does not return a nodeset but returns an error
#' is.null(var_text2_node$result)
#' var_text2_node$error
#'
#' @name safe_xml_find
NULL

#' @rdname safe_xml_find
#' @export
safe_xml_find_one <- purrr::safely(xml2::xml_find_one, otherwise = NA_character_)

#' @rdname safe_xml_find
#' @export
safe_xml_find_all <- purrr::safely(xml2::xml_find_all, otherwise = NA_character_)

#' Safe \code{xml2} extract text function
#'
#' Safe version of \code{xml2::xml_text} function.
#'
#' The safe version is helpful when iterating over a set of XML documents
#' and the schemas are inconsistent.  Rather than throwing errors, the function
#' will return \code{NA_character_} when unable to extract from a node.
#'
#' Suggested resources for XPath are
#' \itemize{
#'   \item \url{https://developer.mozilla.org/en-US/docs/Web/XPath/Functions}
#'   \item \url{http://www.w3schools.com/xsl/xpath_syntax.asp}
#' }
#'
#' @param x A document, node, or node set.
#' @param trim If \code{TRUE} will trim leading and trailing spaces.
#'
#' @return A character vector or \code{NA_character_} if there is an error.
#'
#' @seealso
#' \itemize{
#'   \item \link[xml2]{xml_attr}
#'   \item \link[xml2]{xml_text}
#' }
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
#' var_text <- safe_xml_text(
#'   safe_xml_find_one(xml2::read_xml(xml_doc),
#'                     xpath = "//root_tag/another_tag/var_text")$result)
#'
#' # returns "10" without error
#' var_text$result
#' is.null(var_text$error)
#'
#' var_text2 <- safe_xml_text(
#'   safe_xml_find_one(xml2::read_xml(xml_doc),
#'                     xpath = "//root_tag/another_tag/var_text2")$result)
#'
#' # does not return a value but returns an error
#' is.null(var_text2$result)
#' var_text2$error
#'
#' @export
safe_xml_text <- purrr::safely(xml2::xml_text, otherwise = NA_character_)

#' Safe \code{xml2} extract attr function
#'
#' Safe version of \code{xml2::xml_attr} function.
#'
#' The safe version is helpful when iterating over a set of XML documents
#' and the schemas are inconsistent.  Rather than throwing errors, the function
#' will return \code{NA_character_} when unable to extract from a node.
#'
#' Suggested resources for XPath are
#' \itemize{
#'   \item \url{https://developer.mozilla.org/en-US/docs/Web/XPath/Functions}
#'   \item \url{http://www.w3schools.com/xsl/xpath_syntax.asp}
#' }
#'
#' @param x A document, node, or node set.
#' @param ns Optionally, a named vector giving prefix-url pairs, as produced
#' by \code{\link[xml2]{xml_ns}}. If provided, all names will be explicitly
#' qualified with the ns prefix, i.e. if the element \code{bar} is defined
#' in namespace \code{foo}, it will be called \code{foo:bar}. (And
#' similarly for atttributes). Default namespaces must be given an explicit
#' name.
#' @param attr Name of attribute to extract.
#' @param default Default value to use when attribute is not present.
#'
#' @return A character vector or \code{NA_character_} if there is an error.
#'
#' @seealso
#' \itemize{
#'   \item \link[xml2]{xml_attr}
#'   \item \link[xml2]{xml_text}
#' }
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
#' var_attr <- safe_xml_attr(
#'   safe_xml_find_one(xml2::read_xml(xml_doc),
#'                     xpath = "//root_tag/another_tag/var_attr")$result,
#'   "value")
#'
#' # returns "10" without error
#' var_attr$result
#' is.null(var_attr$error)
#'
#' var_attr2 <- safe_xml_attr(
#'   safe_xml_find_one(xml2::read_xml(xml_doc),
#'                     xpath = "//root_tag/another_tag/var_attr")$result,
#'   "value2")
#'
#' # does not return a value but returns an error
#' is.null(var_attr2$result)
#' var_attr2$error
#'
#' @export
safe_xml_attr <- purrr::safely(xml2::xml_attr, otherwise = NA_character_)


#' Get XML Attribute
#'
#' Return an XML node attribute if provided the document, XPath, and attribute
#' name.
#'
#' Serves as a convenience function.  Utilizes \code{\link{safe_xml_find_one}}
#' and \code{\link{safe_xml_attr}} and will return \code{NA_character_} if an
#' error occurs.
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
  xml_node <- safe_xml_find_one(xml_doc, xpath, ns = xml2::xml_ns(xml_doc))
  xml_attr <- safe_xml_attr(xml_node$result, extract_value)
  return(xml_attr$result)
}


#' Get XML Text
#'
#' Return an XML text value if provided the document and Xpath.
#'
#' Serves as a convenience function.  Utilizes \code{\link{safe_xml_find_one}}
#' and \code{\link{safe_xml_text}} and will return \code{NA_character_} if an
#' error occurs.
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
  xml_node <- safe_xml_find_one(xml_doc, xpath, ns = xml2::xml_ns(xml_doc))
  xml_text <- safe_xml_text(xml_node$result)
  return(xml_text$result)
}

#' XML Extract
#'
#' Extract XML text or an XML attribute via XPath.
#'
#' May be used as a UDF that is part of a dplyr pipeline.  The most simple use
#' is to include \code{xml_extract} as part of a mutate function.  For details
#' see \code{vignette("chunked-invoke-rows-xml")}.
#'
#' A more complex use would be to use as a UDF to parse an arbitrary number of
#' xml text and attribute values from an XML document.  This can be accomplished
#' utilizing a dataframe holding parameter values, \code{purrr::pmap}, and
#' \code{purrr::invoke_rows}.
#'
#' The safe versions of \code{xml2} functions are utilized in
#' \code{xml_extract}.  The safe versions consume errors and is helpful when
#' iterating over a set of XML documents where the schemas are inconsistent.
#' But the safety provided slows down the parsing.
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
#' See \code{vignette("chunked-invoke-rows")} for simple usage.
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
  xml_node <- safe_xml_find_one(xml_doc, xpath, ns = xml2::xml_ns(xml_doc))

  # extract the actual value
  if (extract_type == "attr") {
    xpath_result <- safe_xml_attr(xml_node$result, extract_value)$result
  } else {
    xpath_result <- safe_xml_text(xml_node$result)$result
  }

  # assign a name to the result
  # useful if using with pmap and invoke_rows
  if(!is.null(ret_var_name)) names(xpath_result) <- ret_var_name

  return(xpath_result)
}
