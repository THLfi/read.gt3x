#' @useDynLib read.gt3x
#' @importFrom Rcpp sourceCpp
NULL


#' Read GT3X
#'
#' Read activity samples from a GT3X file as a matrix.
#' Please note that all timestams are in local time (of the device) even though they are
#' represented as POSIXct with GMT timezone.
#'
#' @param path Path to gt3x folder
#' @param asDataFrame convert to an activity_df, see \code{as.data.frame.activity}
#' @param imputeZeroes Impute zeros in case there are missingness? Default is FALSE, in which case
#' the time series will be incomplete in case there is missingness.
#'
#' @return A numeric matrix with 3 columns (X, Y, Z) and the following attributes:
#'  \itemize{
#' \item \code{start_time} :  Start time from info file in \code{POSIXct} format.
#' \item \code{subject_name} : Subject name from info file
#' \item \code{time_zone} : Time zone from info file
#' \item \code{missingness} : Named integer vector. Names are Posixct timestamps and values are the number of missing values.
#' }
#'
#' @examples
#'
#' # first unzip, then read
#' datadir <- gt3x_datapath()
#' gt3xfolders <- unzip.gt3x(datadir)
#' gt3xfile <- gt3xfolders[2]
#' x <- read.gt3x(gt3xfile, imputeZeroes = T)
#' df2 <- as.data.frame(x)
#' head(df2)
#'
#' # temporary unzip, read, convert to a data frame
#' gt3xfile <- gt3x_datapath(1)
#' df <- read.gt3x(gt3xfile, asDataFrame = TRUE)
#' head(df)
#'
#' @family gt3x-parsers
#'
#' @export
read.gt3x <- function(path, verbose = FALSE, asDataFrame = FALSE, imputeZeroes = FALSE, ...) {

  fun_start_time <- Sys.time()

  if(is_gt3x(path)) {
    message("Input is a .gt3x file, unzipping to a temporary location first...")
    path <- unzip.gt3x(path)
  }

  tz  <- "GMT" # used for parsing, times are actually in local timezone
  info <- parse_gt3x_info(path, tz = tz)

  if (verbose)
    print(info)

  samples <- get_n_samples(info)

  message("Parsing GT3X data via CPP.. expected sample size: ", samples)
  logpath <- file.path(path, "log.bin")
  accdata <- parseGT3X(logpath, max_samples = samples,
                       scale_factor = info$`Acceleration Scale`, sample_rate = info$`Sample Rate`,
                       verbose = verbose, impute_zeroes = imputeZeroes, ...)

  attr(accdata, "start_time") <- info[["Start Date"]]
  attr(accdata, "stop_time") <- info[["Stop Date"]]
  attr(accdata, "subject_name") <- info[["Subject Name"]]
  attr(accdata, "time_zone") <- info[["TimeZone"]]
  attr(accdata, "missingness") <- data.frame(time = as.POSIXct(as.numeric(names(attr(accdata, "missingness"))),
                                                               origin = "1970-01-01", tz = tz),
                                             n_missing = attr(accdata, "missingness"))

  message("Done", " (in ",  as.numeric(difftime(Sys.time(), fun_start_time, units = "secs")), " seconds)")

  x <- structure(accdata,
            class = c("activity", class(accdata)))

  if(asDataFrame)
    x <- as.data.frame(x)

  x

}

#' Convert an activity matrix to a data.frame
#'
#' @param activity Object of class 'activity' (returned by read.gt3x)
#'
#' @family gt3x-parsers
#'
#' @return An object of class 'activity_df' which is also a data.frame with the following attributes
#' #'  \itemize{
#' \item \code{subject_name} : Subject name from info file
#' \item \code{time_zone} : Time zone from info file
#' \item \code{missingness} : Data frame with timestamps and the number of missing values associated.
#' }
#'
#' @export
as.data.frame.activity <- function(activity) {
  options(digits = 15, digits.secs = 3)
  tz <- "GMT" # used for parsing, timestamps are actually in local time
  start_time = as.numeric(attr(activity, "start_time"), tz = tz)
  time_index <- attr(activity, "time_index")
  sample_rate <- attr(activity, "sample_rate")

  message("Converting to a data.frame ...")
  df <- activityAsDataFrame(activity, time_index, start_time, sample_rate)
  df$time <- as.POSIXct(df$time, origin = "1970-01-01", tz = tz)
  message("Done")
  structure(df,
            class = c("activity_df", class(df)),
            subject_name = attr(activity, "subject_name"),
            time_zone = attr(activity, "time_zone"),
            missingness = attr(activity, "missingness"))
}

