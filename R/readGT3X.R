#' @useDynLib read.gt3x
#' @importFrom Rcpp sourceCpp
NULL


#' Convert NET ticks to POSIXct datetime
#'
#' @details
#' reference: \url{https://stackoverflow.com/questions/35240874/r-net-ticks-to-timestamp-in-r}
#'
#' @family parser-utils
ticks2datetime <- function(ticks, tz) {
  ticks <- as.numeric(ticks)
  seconds <- ticks / 1e7
  datetime <- as.POSIXct(seconds, origin='0001-01-01', tz=tz)
  datetime
}

#' Parse GT3X info.txt file
#'
#' @param path Path to a .gt3x file or an unzipped gt3x directory
#'
#' @family gt3x-parsers
parse_gt3x_info <- function(path) {
  if(is_gt3x(path))
    path <- unzip.gt3x(path, files = "info.txt")
  infotxt <- readLines(file.path(path, "info.txt"))
  infotxt <- strsplit(infotxt, split = ": ")
  infomatrix <- do.call("rbind", infotxt)
  values <- infomatrix[, 2]
  names(values) <- infomatrix[, 1]
  info <- as.list(values)
  info$`Sample Rate` <- as.numeric(info$`Sample Rate`)
  info$`Start Date` <- ticks2datetime(info$`Start Date`, tz = info$`TimeZone`)
  info$`Last Sample Time` <- ticks2datetime(info$`Last Sample Time`, tz = info$`TimeZone`)
  info$`Download Date` <- ticks2datetime(info$`Download Date`, tz = info$`TimeZone`)
  info$`Acceleration Scale` <- as.numeric(info$`Acceleration Scale`)
  structure(info, class = c("gt3x_info", class(info)))
}

#' Print the contents of the info.txt file in a gt3x folder
#'
#' @param x gt3x_info object returned by parse_gt3x_info()
#'
#' @family gt3x-parser
#'
print.gt3x_info <- function(x) {
  cat("GT3X information\n")
  str(x, give.head = FALSE, no.list=TRUE)
}

#' Calculate the expected activity sample size from start time and last sample time in the info.txt of a gt3x directory
#'
#' @family parser-utils
get_n_samples<- function(info) {
  start <- info[["Start Date"]]
  end <- info[["Last Sample Time"]]
  rate <- info[["Sample Rate"]]
  seqs <- as.numeric(difftime(end, start, units = "secs"))
  seqs*rate
}

#' Read GT3X
#'
#' Read activity samples from a GT3X file as a matrix
#'
#' @param path Path to gt3x folder
#'
#' @examples
#'
#' # first unzip, then read
#' datadir <- gt3x_datapath()
#' gt3xfolders <- unzip.gt3x(datadir)
#' x <- readGT3X(gt3xfolders[1])
#' df <- as.data.frame(x)
#'
#' # temporary unzip and read
#' gt3xfile <- gt3x_datapath(1)
#' x <- readGT3X(gt3xfile)
#'
#' @family gt3x-parser
#'
#' @export
readGT3X <- function(path, verbose = FALSE, ...) {

  fun_start_time <- Sys.time()

  if(is_gt3x(path)) {
    message("Input is a .gt3x file, unzipping to a temporary location first...")
    path <- unzip.gt3x(path)
  }

  info <- parse_gt3x_info(path)

  if (verbose)
    print(info)

  samples <- get_n_samples(info)

  message("Parsing GT3X data via CPP.. expected sample size: ", samples)
  logpath <- file.path(path, "log.bin")
  accdata <- parseGT3X(logpath, max_samples = samples, scale_factor = info$`Acceleration Scale`, sample_rate = info$`Sample Rate`, verbose = verbose, ...)

  attr(accdata, "start_time") = as.POSIXct(attr(accdata, "start_time"), origin = "1970-01-01")

  message("Done", " (in ",  as.integer(difftime(Sys.time(), fun_start_time, units = "secs")), " seconds)")

  structure(accdata,
            class = c("activity", class(accdata)))

}

#' Convert an activity matrix to a data.frame
#'
#' @param activity Object of class 'activity' (returned by readGT3X)
#'
#' @family gt3x-parser
#'
#' @export
as.data.frame.activity <- function(activity) {
  options(digits = 15, digits.secs = 3)
  start_time = as.numeric(attr(activity, "start_time"))
  time_index <- attr(activity, "time_index")
  sample_rate <- attr(activity, "sample_rate")
  df <- activityAsDataFrame(activity, time_index, start_time, sample_rate)
  class(df$time) <- "POSIXct"
  df
}
