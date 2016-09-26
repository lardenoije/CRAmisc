context("Unzip files")

test_that("Unzip all gzipped files in a directory", {
  withr::with_dir(
    new = tempdir(),
    code = {
      # create two small tables to write out as files
      a_df <- tibble::tribble(
        ~a, ~b, ~c,
        1, 2, 3,
        4, 5, 6,
        7, 8, 9
      )

      d_df <- tibble::tribble(
        ~d, ~e, ~f,
        11, 22, 33,
        44, 55, 66,
        77, 88, 99
      )

      # make a new input directory
      system2(command = "mkdir",
              args = "in_dir_gz",
              stdout = NULL)
      setwd("in_dir_gz")

      # write csv files
      readr::write_csv(a_df, "a.csv")
      readr::write_csv(d_df, "d.csv")

      # convert into gzip files
      system2(command = "gzip",
              args = c("a.csv", "d.csv"),
              stdout = NULL)

      # make a new output directory
      setwd("..")
      system2(command = "mkdir",
              args = "out_dir_gz",
              stdout = NULL)

      # unzip all files
      unzip_dir("in_dir_gz", "out_dir_gz", "gz")

      expect_equal(readr::read_csv("out_dir_gz/a.csv"), a_df)
      expect_equal(readr::read_csv("out_dir_gz/d.csv"), d_df)
    }
  )
})

test_that("Unzip all zipx files in a directory", {
  withr::with_dir(
    new = tempdir(),
    code = {
      # create two small tables to write out as files
      a_df <- tibble::tribble(
        ~a, ~b, ~c,
        1, 2, 3,
        4, 5, 6,
        7, 8, 9
      )

      d_df <- tibble::tribble(
        ~d, ~e, ~f,
        11, 22, 33,
        44, 55, 66,
        77, 88, 99
      )

      # make a new input directory
      system2(command = "mkdir",
              args = "in_dir_zipx",
              stdout = NULL)
      setwd("in_dir_zipx")

      # write csv files
      readr::write_csv(a_df, "a.csv")
      readr::write_csv(d_df, "d.csv")

      # convert into zipx files
      system2(command = "7z",
              args = c("a", "a.csv.zipx", "a.csv"),
              stdout = NULL)
      system2(command = "7z",
              args = c("a", "d.csv.zipx", "d.csv"),
              stdout = NULL)

      # make a new output directory
      setwd("..")
      system2(command = "mkdir",
              args = "out_dir_zipx",
              stdout = NULL)

      # unzip all files
      unzip_dir("in_dir_zipx", "out_dir_zipx", "zipx")

      expect_equal(readr::read_csv("out_dir_zipx/a.csv"), a_df)
      expect_equal(readr::read_csv("out_dir_zipx/d.csv"), d_df)
    }
  )
})

test_that("Unzip all zip files in a directory regardless of case of file extension", {
  withr::with_dir(
    new = tempdir(),
    code = {
      # create two small tables to write out as files
      a_df <- tibble::tribble(
        ~a, ~b, ~c,
        1, 2, 3,
        4, 5, 6,
        7, 8, 9
      )

      d_df <- tibble::tribble(
        ~d, ~e, ~f,
        11, 22, 33,
        44, 55, 66,
        77, 88, 99
      )

      # make a new input directory
      system2(command = "mkdir",
              args = "in_dir_zip",
              stdout = NULL)
      setwd("in_dir_zip")

      # write csv files
      readr::write_csv(a_df, "a.csv")
      readr::write_csv(d_df, "d.csv")

      # convert into zipx files
      system2(command = "zip",
              args = c("a.csv.ZIP", "a.csv"),
              stdout = NULL)
      system2(command = "zip",
              args = c("d.csv.ZIP", "d.csv"),
              stdout = NULL)

      # make a new output directory
      setwd("..")
      system2(command = "mkdir",
              args = "out_dir_zip",
              stdout = NULL)

      # unzip all files
      unzip_dir("in_dir_zip", "out_dir_zip", "ZIP")

      expect_equal(readr::read_csv("out_dir_zip/a.csv"), a_df)
      expect_equal(readr::read_csv("out_dir_zip/d.csv"), d_df)
    }
  )
})
