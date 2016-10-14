context("Utilities")

# trick is to find something that only works in R or Perl regular expressions
# \\< in R ==> matches empty string at the beginning of a word
# \\< in Perl ==> matches the literal character <

test_that("%matches% returns logical", {
  needle <- "\\<as"
  haystack <- " asdf"

  expect_equal(haystack %matches% needle, TRUE)
})

test_that("%matches% works with dataframe filtering", {
  virginica_char <- iris %>%
    dplyr::filter(Species %matches% "^vir") %>%
    dplyr::distinct(Species) %>%
    .[1,] %>%
    as.character()

  expect_equal(virginica_char, "virginica")
})

test_that("%pmatches% returns logical", {
  needle <- "\\<as"
  haystack <- " asdf"

  expect_equal(haystack %pmatches% needle, FALSE)
})

test_that("%pmatches% works with dataframe filtering", {
  virginica_char <- iris %>%
    dplyr::filter(Species %pmatches% "^vir") %>%
    dplyr::distinct(Species) %>%
    .[1,] %>%
    as.character()

  expect_equal(virginica_char, "virginica")
})