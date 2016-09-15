#' Unzip all files in a directory
#'
#' Unzip all files in a given directory, identified by extension, placing the
#' unzipped file in a separate output directory.
#'
#' \code{unzip_dir} makes system calls to \code{gunzip}, \code{unzip}, and
#' \code{unar}.  Each of these need to be on the system \code{PATH} and able to
#' be called from \link[base]{system2}.  Calls have only been tested on OSX at
#' this time.
#'
#' On OSX, \code{unar} may be installed via \href{http://brew.sh}{Homebrew}
#' using the install command \code{brew install unar}.
#'
#' Finally, \code{unzip_dir} checks whether or not the output directory already
#' contains the file to be unzipped.  If the file already exists, then unzipping
#' is skipped for the file.  This allows one to re-run the function multiple
#' times without continually unzipping all files in the input directory.
#'
#' @param in_dir Input directory containing the zipped files to convert.
#' @param out_dir Output directory to place the unzipped files.
#' @param ext Zipped file extension.  The extensions supported are
#' \itemize{
#'   \item \code{gz} - gzipped files, unzipped with \code{gunzip}
#'   \item \code{zip} - zip files, unzipped with \code{unzip}
#'   \item \code{zipx} - zipx or archive files, unzipped with \code{unar}
#' }
#'
#' @return Invisibly returns a character vector of the zipped file basenames.
#'
#' @examples
#' withr::with_dir(
#'   new = tempdir(),
#'   code = {
#'
#'     # create two small tables to write out as files
#'     a_df <- tibble::tribble(
#'       ~a, ~b, ~c,
#'       1, 2, 3,
#'       4, 5, 6,
#'       7, 8, 9
#'     )
#'
#'     d_df <- tibble::tribble(
#'       ~d, ~e, ~f,
#'       11, 22, 33,
#'       44, 55, 66,
#'       77, 88, 99
#'     )
#'     # make a new input directory
#'     system2(command = "mkdir",
#'             args = "in_dir")
#'     setwd("in_dir")
#'
#'     # write csv files
#'     readr::write_csv(a_df, "a.csv")
#'     readr::write_csv(d_df, "d.csv")
#'
#'     # convert into gzip files
#'     system2(command = "gzip",
#'             args = c("a.csv", "d.csv"))
#'
#'     # make a new output directory
#'     setwd("..")
#'     system2(command = "mkdir",
#'             args = "out_dir")
#'
#'     # unzip all files
#'     unzip_dir("in_dir", "out_dir", "gz")
#'
#'     # list all files
#'     system2(command = "ls",
#'             args = c("-l", "out_dir"))
#'   }
#' )
#' @export
unzip_dir <- function(in_dir, out_dir, ext) {
  zip_basename <- list.files(path = in_dir,
                             pattern = paste0("\\.", ext, "$"),
                             recursive = FALSE) %>%
    strsplit(paste0("\\.", ext, "$")) %>%
    unlist()

  already_unzipped <- list.files(path = out_dir,
                                 recursive = FALSE)

  to_unzip <- purrr::discard(zip_basename, ~ .x %in% already_unzipped)

  if (ext == "gz") {
    purrr::walk(
      .x = to_unzip,
      .f = function(z) system2(command = "gunzip",
                               args = c("-c",
                                        shQuote(file.path(in_dir,
                                                          paste0(z, ".gz")))),
                               stdout = file.path(out_dir, z))
    )
  } else if (ext == "zipx") {
    purrr::walk(
      .x = to_unzip,
      .f = function(z) system2(command = "unar",
                               args = c(shQuote(file.path(in_dir,
                                                          paste0(z, ".zipx"))),
                                        "-o",
                                        shQuote(file.path(out_dir))))
    )
  } else if (ext == "zip") {
    purrr::walk(
      .x = to_unzip,
      .f = function(z) system2(command = "unzip",
                               args = c(shQuote(file.path(in_dir,
                                                          paste0(z, ".zip"))),
                                        "-d",
                                        shQuote(file.path(out_dir))))
    )
  } else {
    stop(paste0("The extension ", ext, "is not supported at this time."))
  }
}