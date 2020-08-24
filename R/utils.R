
#' Check if files are .gt3x files
#'
#' @param path Path(s) to file(s)
#'
#' @details
#' Checks if files have a .gt3x file extension
#'
#' @return
#' Logical vector of the same length as path, which is TRUE if the corresponding path is a .gt3x file.
#'
#' @family file manipulations
#' @export
#'
#' @examples
#'
#' is_gt3x("test.gt3x") # TRUE
#' is_gt3x("test") # FALSE
#' is_gt3x(NULL)
#'
is_gt3x <- function(path) {
  if (length(path) == 0) return(FALSE)
  sapply(path, function(f) grepl("\\.gt3x$", f))
}

unzip_zipped_gt3x = function(path, cleanup = TRUE) {
  if (length(path) == 0) return(path)
  stopifnot(length(path) == 1)
  exts = sapply(path, tools::file_ext)
  reg_exts = exts
  exts = tolower(exts)
  unzip_these = exts %in% c("gz", "bz", "bz2", "xz")
  # don't decompress if the file doesn't exist
  fe = file.exists(path)
  fe_before = file.exists(sub(paste0("[.]", reg_exts, "$"), "", path))
  if (any(unzip_these & fe)) {
    zipped_files = path[unzip_these & fe]
    zip_exts = exts[unzip_these & fe]
    zip_outfiles = mapply(function(x, y) {
      FUN = switch(y,
                   bz = bzfile,
                   bz2 = bzfile,
                   gz = gzfile,
                   xz = xzfile)
      R.utils::decompressFile(
        x,
        ext = y,
        FUN = FUN,
        remove = FALSE,
        overwrite = TRUE,
        temporary = TRUE)
    }, zipped_files, zip_exts)
    path[unzip_these & fe] = zip_outfiles
  }

  attr(path, "remove") = unzip_these & cleanup & !fe_before
  path
}


#' List full paths to all gt3x files in a directory
#'
#' @examples
#' path <-
#'   system.file(
#'     "extdata",
#'     package = "read.gt3x")
#' list_gt3x(path)
#' \dontrun{
#' list_gt3x(gt3x_datapath())
#' }
#' @param path Path(s) to file(s)
#'
#' @family file manipulations
#'
#' @export
list_gt3x <- function(path) {
  files <- list.files(path = path, full.names = TRUE)
  gt3xfiles <- files[is_gt3x(files)]
  gt3xfiles
}


#' Check if a .gt3x file or unzipped gt3x directory has both log.bin adn info.txt
#'
#' @family gt3x-utils
#' @rdname is_gt3x
#' @param verbose print diagnostic messages
#' @export
#' @examples
#' have_log_and_info(tempfile(), verbose = TRUE)
have_log_and_info <- function(path, verbose = TRUE) {
  if (is_gt3x(path)) {
    filenames <- unzip(path, list = TRUE)$Name
  } else {
    filenames <- list.files(path)
  }
  haslog <- have_log(path, verbose)
  hasinfo <- have_info(path, verbose)
  if (!haslog & verbose) {
    message(path, " doesn't contain log.bin")
  }
  if (!hasinfo & verbose) {
    message(path, " doesn't contain info.txt")
  }
  return(haslog & hasinfo)
}



have_info <- function(path, verbose = TRUE) {
  if (is_gt3x(path)) {
    filenames <- unzip(path, list = TRUE)$Name
  } else {
    filenames <- list.files(path)
  }
  "info.txt" %in% filenames
}


have_log <- function(path, verbose = TRUE) {
  if (is_gt3x(path)) {
    filenames <- unzip(path, list = TRUE)$Name
  } else {
    filenames <- list.files(path)
  }
  "log.bin" %in% filenames
}


#' Convert NET ticks to POSIXct datetime
#'
#' @details
#' reference: \url{https://stackoverflow.com/questions/35240874/r-net-ticks-to-timestamp-in-r}
#' @param ticks values in NET ticks format
#' @param tz timezone, passed to \code{\link{as.POSIXct}}
#' @family gt3x-utils
ticks2datetime <- function(ticks, tz = "GMT") {
  ticks <- as.numeric(ticks)
  seconds <- ticks / 1e7
  datetime <- as.POSIXct(seconds, origin = '0001-01-01', tz = tz)
  datetime
}

#' Calculate the expected activity sample size from start time and last sample time in the info.txt of a gt3x directory
#'
#' @family gt3x-utils
#' @param x info out from \code{\link{parse_gt3x_info}}
#' @export
get_n_samples <- function(x) {
  start <- x[["Start Date"]]
  end <- x[["Last Sample Time"]]
  rate <- x[["Sample Rate"]]
  if (length(end) == 0) {
    end = x[["Stop Date"]]
  }
  seqs <- as.numeric(difftime(end, start, units = "secs"))
  seqs*rate
}
