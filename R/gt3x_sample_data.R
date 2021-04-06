#' Path to read.gt3x package sample data
#'
#' @param index Integer. The index of a sample file to retrieve.
#' If NULL (default) the path to the directory
#' including the sample files will be returned.
#' @param verbose print diagnostic messages
#'
#' @examples
#' \dontrun{
#' dir <- gt3x_datapath()
#' gt3x_filename <- gt3x_datapath(1)
#' stopifnot(!is.na(gt3x_datapath(2)))
#' }
#'
#' @family file manipulations
#'
#' @export
gt3x_datapath <- function(index = NULL, verbose = TRUE) {
  homedir <- tempdir()
  datadir <- file.path(homedir, ".read.gt3x-data")
  if (!dir.exists(datadir)) {
    dir.create(datadir)
  }
  filenames <- gt3x_filename(index)
  for (i in seq_along(filenames)) {
    if (!file.exists(file.path(datadir, filenames[i]))) {
      gt3x_download(
        url = paste0(gt3x_dataurl(), "/", filenames[i], ".zip"),
        exdir = datadir,
        verbose = verbose)
    }
  }
  if (!is.null(index)) {
    files <- file.path(datadir, filenames)
    return(files)
  }
  datadir
}


#' Download and unzip a zipped `gt3x` file
#'
#' @family sample-data
#' @param url url of the file to download
#' @param exdir directory to extract the zip file
#' @param verbose print diagnostic messages
#'
#' @importFrom utils download.file str unzip
#' @export
#' @return file path of \code{exdir}
#'
gt3x_download <- function(url, exdir, verbose = TRUE) {
  if (verbose) {
    message("Downloading gt3x sample data from ", url)
  }
  temp <- tempfile()
  on.exit(unlink(temp))
  old <- options()         # code line i
  on.exit(options(old), add = TRUE)    # code line i+1
  options(timeout = 300)
  res = download.file(url, temp, method = "auto",
                      quiet = !verbose, mode = "wb")
  if (res != 0) {
    warning("File download did exit with 0 code, may not fully download")
  }
  unzip(temp, exdir = exdir)
  attr(exdir, "result") = res
  return(exdir)
}

#' Get url of gt3x sample file
#'
#' @family sample-data
#' @param filename file to grab to make url
#' @param index The index of a sample file to retrieve, passed to
#' \code{\link{gt3x_filename}}
#' @export
#' @return file path
gt3x_url <- function(index = NULL, filename = NULL) {
  dataurl <- gt3x_dataurl()
  if (is.null(filename)) {
    filename <- gt3x_filename(index, zipped = TRUE)
  }
  file.path(dataurl, filename)
}

#' Get url of github release
#'
#' @family sample-data
#' @param version release version
#' @param baseurl URL for GitHub release
#' @return URL to file
gt3x_dataurl <- function(
  version = "v1.0",
  baseurl = "https://github.com/THLfi/read.gt3x/releases/download") {
  url <- paste(baseurl, version, sep ="/")
  url
}

#' @rdname gt3x_datapath
#' @param zipped do the files have a \code{.zip} extension
#' @return Character vector of files
#' @export
#' @examples
#' testthat::expect_error(gt3x_filename(100))
#' testthat::expect_error(gt3x_filename(0))
gt3x_filename <- function(index = NULL, zipped = FALSE) {
  files <- c("EE_left_29.5.2017-05-30.gt3x", "SS_left_19.5.2017-05-22.gt3x")
  if (zipped) {
    files <- paste0(files, ".zip")
  }
  if (is.null(index)) {
    return(files)
  }
  if (index > length(files)) {
    stop("Index is larger than the number of available files")
  }
  if (index < 1) {
    stop("Index can't be less than 1")
  }
  files[index]
}
