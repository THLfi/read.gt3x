#' @useDynLib read.gt3x
#' @importFrom Rcpp sourceCpp
NULL


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
