context("XML")

# get environment variables
# use environment variables to prevent being exposed publicly
# see also
#   https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html
#   http://blog.revolutionanalytics.com/2015/11/how-to-store-and-use-authentication-details-with-r.html
# httr_cainfo <- Sys.getenv("TEST_HTTR_CAINFO")

# setup test XML document
xml_doc <- "
<root_tag>
  <another_tag>
    <var_text>10</var_text>
    <var_attr value=\"10\"></var_attr>
  </another_tag>
</root_tag>
"

test_that("get_xml_attr retrieves an attribute value", {
  expect_equal(get_xml_attr(xml_doc,
                            "//root_tag/another_tag/var_attr",
                            "value"), "10")
})

test_that("get_xml_attr returns NA when an attribute is missing", {
  expect_equal(get_xml_attr(xml_doc,
                            "//root_tag/another_tag/missing_attr",
                            "value"), NA_character_)
})

test_that("get_xml_text retrieves a text value", {
  expect_equal(get_xml_text(xml_doc,
                            "//root_tag/another_tag/var_text"), "10")
})

test_that("get_xml_text returns NA when a text value is missing", {
  expect_equal(get_xml_text(xml_doc,
                            "//root_tag/another_tag/missing_text"),
               NA_character_)
})

# xml_extract
test_that("xml_extract returns an error when extract_type is not \"text\" or \"attr\"", {
  expect_error(xml_extract(x = xml_doc,
                           xpath = "//root_tag/another_tag",
                           extract_type = "something_else"))
})

test_that("xml_extract retrieves an attribute value", {
  expect_equal(xml_extract(x = xml_doc,
                           xpath = "//root_tag/another_tag/var_attr",
                           extract_type = "attr",
                           extract_value = "value"), "10")
})

test_that("xml_extract returns NA when an attribute is missing", {
  expect_equal(xml_extract(x = xml_doc,
                           xpath = "//root_tag/another_tag/missing_attr",
                           extract_type = "attr",
                           extract_value = "value"), NA_character_)
})

test_that("xml_extract retrieves a text value", {
  expect_equal(xml_extract(x = xml_doc,
                           xpath = "//root_tag/another_tag/var_text",
                           extract_type = "text"), "10")
})

test_that("xml_extract returns NA when a text value is missing", {
  expect_equal(xml_extract(x = xml_doc,
                           xpath = "//root_tag/another_tag/missing_text",
                           extract_type = "text"), NA_character_)
})

test_that("xml_extract returns a value without a name if ret_var_name is NULL", {
  xml_result <- xml_extract(x = xml_doc,
                            xpath = "//root_tag/another_tag/var_text",
                            extract_type = "text")
  expect_equal(names(xml_result), NULL)
})

test_that("xml_extract returns a value with a name if ret_var_name is set", {
  xml_result <- xml_extract(x = xml_doc,
                            xpath = "//root_tag/another_tag/var_text",
                            extract_type = "text",
                            ret_var_name = "blah")  # ;-)
  expect_equal(names(xml_result), "blah")
})
