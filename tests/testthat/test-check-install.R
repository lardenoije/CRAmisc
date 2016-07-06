context("Check install")

# get environment variables
# use environment variables to prevent being exposed publicly
# see also
#   https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html
#   http://blog.revolutionanalytics.com/2015/11/how-to-store-and-use-authentication-details-with-r.html
httr_cainfo <- Sys.getenv("TEST_HTTR_CAINFO")

# test httr::http_error for use with the prior_install test
safe_http_error <- purrr::safely(httr::http_error)
http_error_test <- safe_http_error("https://cran.rstudio.com/src/contrib/Archive/")

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
      dplyr::distinct() %>%
      dplyr::select(Package) %>%
      as.character()

    expect_match(installed_results, "pkgKitten")
  })
})

test_that("check_install installs a missing package (CRAN mirror provided)", {
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
                              code = {check_install("pkgKitten",
                                                    repos = "https://cloud.r-project.org")}
      )
    })
    # what is installed
    installed_results <- installed.packages() %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      dplyr::distinct() %>%
      dplyr::select(Package) %>%
      as.character()

    expect_match(installed_results, "pkgKitten")
  })
})

test_that("prior_install installs an older version of a package", {
  skip_if_not(!(is.null(http_error_test$result) &
              httr_cainfo == "" &
              grepl("Peer certificate cannot be authenticated with given CA certificates",
                    http_error_test$error)),
              message = paste0("Please create the environment variable TEST_HTTR_CAINFO ",
                               "within .Renviron.  Set the value of TEST_HTTR_CAINFO to the ",
                               "appropriate CA file."))
  # setup a temporary libpath and restore the normal libpath upon completion
  withr::with_temp_libpaths({
    # install package and...
    #   suppress all messages and output during the installation process
    #     note that this is not helpful if an error is thrown
    sink_file <- ifelse(is_windows(), "NUL", "/dev/null")
    withr::with_message_sink(new = sink_file,
                             code = {
      withr::with_output_sink(new = sink_file,
                              code = {
        # pkgKitten: small, no {imports, compilation}
        if(httr_cainfo != "") {
          prior_install("pkgKitten", "0.1.0", cainfo = httr_cainfo)
        } else {
          prior_install("pkgKitten", "0.1.0")
        }
      })
    })
    # what is installed
    installed_results <- installed.packages() %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      dplyr::distinct() %>%
      dplyr::filter(Package == "pkgKitten")

    pkg_name <- installed_results %>%
      dplyr::select(Package) %>%
      as.character()

    expect_match(pkg_name, "pkgKitten")

    # package version
    pkg_version <- installed_results %>%
      dplyr::select(Version) %>%
      as.character()

    expect_match(pkg_version, "0.1.0")
  })
})