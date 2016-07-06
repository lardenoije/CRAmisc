context("Check install")

# tests
test_that("check_install returns message for already installed package", {
  expect_output(check_install("utils"), "utils already installed")
})

test_that("check_install can process a vector", {
  pkgs_vector <- c("utils", "tools")
  expect_output(check_install(pkgs_vector),
                "utils already installed\ntools already installed")
})

test_that("check_install can process a list", {
  pkgs_list <- list("utils", "tools")
  expect_output(check_install(pkgs_list),
                "utils already installed\ntools already installed")
})

test_that("check_install installs a missing package (no CRAN mirror provided)", {
  # setup a temporary libpath and restore the normal libpath upon completion
  withr::with_temp_libpaths({
    # install package and...
    #   suppress all messages and output during the installation process
    #     note that this is not helpful if an error is thrown
    sink_file <- ifelse(is_windows(), "NUL", "/dev/null")
    withr::with_message_sink(new = sink_file,
                             code = {
      withr::with_output_sink(new = sink_file,
                                      # pkgKitten: small, no {imports, compilation}
                              code = {check_install("pkgKitten")}
      )
    })
    # what is installed
    installed_results <- installed.packages() %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      .[, "Package", drop = FALSE] %>% as.character()

    expect_match(installed_results, "pkgKitten")
  })
})

