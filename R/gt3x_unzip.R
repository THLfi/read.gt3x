
#' Unzip a single gt3x file
#'
#' A .gt3x file is a zipped archive with two files: log.bin and info.txt.
#' This function unzips the contents of the archive to a single folder.
#' This is a helper for unzip.gt3x()
#'
#' @param path Path to a .gt3x file
#' @param dirname The name of the resulting directory where the content of
#' `path` are extracted.
#' Default is the name of the input file, sans the .gt3x extension.
#' @param location A path to an output directory. Default is a `tempdir`.
#' @param files The names of files to extract. Default is `info.txt` and `log.bin`
#' @param remove_original Remove the zip file after unzipping?
#' @param check_structure check to see if the structure is right for the file
#' @param verbose print diagnostic messages
#'
#' @export
#'
unzip_single_gt3x <- function(
  path,
  dirname =  basename(gsub(".gt3x$| ", "", path)),
  location = tempdir(),
  files = c("info.txt", "log.bin"),
  remove_original = FALSE,
  check_structure = TRUE,
  verbose = TRUE) {

  if (verbose) {
    message("Unzipping ", path)
  }

  if (!is_gt3x(path)) {
    message(path, " is a not a .gt3x file. Unzipping failed")
    return(NULL)
  }

  if (check_structure) {
    if (!have_log_and_info(path)) {
      message(path, " did not contain both log.bin and ",
              "info.txt. Unzipping failed.")
      return(NULL)
    }
  }

  exdir <- file.path(location, dirname)
  if (check_structure) {
    extractedpaths <- unzip(path, files = files, exdir = exdir)
  } else {
    suppressWarnings({
      extractedpaths <- unzip(path, files = files, exdir = exdir)
    })
  }
  stopifnot(length(extractedpaths) > 0)
  if (verbose) {
    message(
      paste0(
        " === ",
        paste(files, collapse = ", "),
        " extracted to ", exdir)
    )
  }
  if (remove_original) {
    if (verbose) {
      message("Removing original zipfile...")
    }
    removed <- file.remove(path)
    if (removed & verbose) {
      message("Removed ", path)
    }
  }
  exdir
}



#' Unzip gt3x files
#'
#' unzip.gt3x() makes it convenient to unzip multiple .gt3x files.
#'
#' @param path One of the following: (1) A path to a directory with
#' .gt3x files in which case they are all unzipped, or
#' (2) A character vector of direct paths to .gt3x files.
#' @param verbose print diagnostic messages
#' @param ... arguments to pass to \code{\link{unzip_single_gt3x}}
#'
#' @details
#'  A .gt3x file is a zipped directory with two files: log.bin and info.txt.
#'  This function simply unzips the contents of the directories.
#'
#' @return
#' Returns a vector of paths to unzipped gt3x folders.
#'
#' @family file manipulations
#'
#' @examples
#'
#' gt3xfile <-
#'   system.file(
#'     "extdata", "TAS1H30182785_2019-09-17.gt3x",
#'     package = "read.gt3x")
#' gt3xdirs <- unzip.gt3x(gt3xfile)
#' \dontrun{
#' # unzip a single .gt3x file
#' path <- gt3x_datapath(1)
#' gt3xdir <- unzip.gt3x(path)
#'
#' # unzip multiple .gt3x files
#' dir <- gt3x_datapath()
#' gt3xdirs <- unzip.gt3x(dir)
#' }
#' tfile = tempfile()
#' testthat::expect_error(unzip.gt3x(c(dir, tfile)))
#' testthat::expect_error(unzip.gt3x(c("", "")))
#'
#' @export
unzip.gt3x <- function(path, verbose = TRUE, ...) {
  if (length(path) == 1 & !is_gt3x(path[1])) {
    gt3xfiles <- list_gt3x(path)
  } else {
    gt3xfiles <- path
    if (!all(is_gt3x(gt3xfiles))) {
      stop("Some or all of the filepaths do not have a .gt3x extension")
    }
  }

  n <- length(gt3xfiles)
  if (n < 1) {
    stop("No .gt3x files found")
  }

  args <- list(...)
  location <- args$location
  if (is.null(location)) {
    location <- tempdir()
  }
  if (verbose) {
    message("Unzipping gt3x data to ", location)
  }

  result_paths <- vector("character", n)
  for (i in seq_len(n)) {
    if (verbose) {
      message(i, "/", n, " ", sep = "")
    }
    result_paths[i] <- unzip_single_gt3x(
      gt3xfiles[i], verbose = verbose,
      ...)
  }
  result_paths
}
