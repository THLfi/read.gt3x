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
#' @param ... additional arguments to pass to \code{parseGT3X} C++ code
#' @param verbose print diagnostic messages
#'
#' @note
#'
#' The timestamps in the .gt3x data format are saved in .NET format, which is
#'  nanoseconds in local time since 0001-01-01.
#' This is a bit tricky to parse into an R datetime format. DateTimes are
#' therefore represented as POSIXct format with the 'GMT' timezone attribute,
#'  which is false; the datetime actually
#' represents local time.
#'
#' @return A numeric matrix with 3 columns (X, Y, Z) and the following attributes:
#'  \itemize{
#' \item \code{start_time} :  Start time from info file in \code{POSIXct} format.
#' \item \code{subject_name} : Subject name from info file
#' \item \code{time_zone} : Time zone from info file
#' \item \code{missingness} : Named integer vector. Names are Posixct
#' timestamps and values are the number of missing values.
#' }
#'
#' @examples
#'
#' # first unzip, then read
#' datadir <- gt3x_datapath()
#' gt3xfolders <- unzip.gt3x(datadir)
#' gt3xfile <- gt3xfolders[2]
#' x <- read.gt3x(gt3xfile, imputeZeroes = TRUE, asDataFrame = FALSE,
#' verbose = TRUE)
#' df2 <- as.data.frame(x)
#' head(df2)
#' rm(x); gc(); gc()
#' rm(df2); gc()
#'
#' # temporary unzip, read, convert to a data frame
#' gt3xfile <- gt3x_datapath(1)
#' memory.limit()
#' df <- read.gt3x(gt3xfile, asDataFrame = FALSE, verbose = 2)
#' head(df)
#'
#' rm(df)
#'
#' \dontrun{
#' url = "https://github.com/THLfi/read.gt3x/files/3522749/GT3X%2B.01.day.gt3x.zip"
#' destfile = tempfile(fileext = ".zip")
#' dl = download.file(url, destfile = destfile)
#' gt3x_file = unzip(destfile, exdir = tempdir())
#' gt3x_file = gt3x_file[!grepl("__MACOSX", gt3x_file)]
#' path = gt3x_file
#'
#' res = read.gt3x(path)
#' }
#' @family gt3x-parsers
#'
#' @export
read.gt3x <- function(path, verbose = FALSE, asDataFrame = FALSE,
                      imputeZeroes = FALSE, ...) {

  fun_start_time <- Sys.time()

  has_info = have_info(path)
  if (has_info) {
    info = parse_gt3x_info(path)
  }
  files = c("info.txt", "log.bin")
  if (has_info) {
    is_old_version = old_version(info)
    if (is_old_version) {
      files = c("info.txt",
                "activity.bin",
                "lux.bin")
    }
  }

  if (is_gt3x(path)) {
    if (verbose) {
      message(paste0("Input is a .gt3x file, unzipping to a ",
                     "temporary location first..."))
    }
    path <- unzip.gt3x(path, verbose = verbose, files = files,
                       check_structure = !is_old_version)
  }

  tz  <- "GMT" # used for parsing, times are actually in local timezone
  info <- parse_gt3x_info(path, tz = tz)
  if (is_old_version) {
    if (length(info$`Acceleration Scale`) == 0) {
      info$`Acceleration Scale` = 341L
    }
  }
  if (verbose) {
    print(info)
  }

  samples <- get_n_samples(info)

  if (verbose) {
    message("Parsing GT3X data via CPP.. expected sample size: ", samples)
  }
  if (!is_old_version) {
    logpath <- file.path(path, "log.bin")
    accdata <- parseGT3X(
      logpath, max_samples = samples,
      scale_factor = info$`Acceleration Scale`,
      sample_rate = info$`Sample Rate`,
      verbose = as.logical(verbose),
      impute_zeroes = imputeZeroes, ...)
    if (verbose > 1) {
      message("Activity data now in R")
    }
  } else {
    if (verbose) {
      message("Using NHANES-GT3X format - older format")
    }
    act_path <- file.path(path, "activity.bin")
    accdata <- parseActivityBin(
      act_path, max_samples = samples,
      scale_factor = info$`Acceleration Scale`,
      sample_rate = info$`Sample Rate`,
      verbose = as.logical(verbose),
      debug = FALSE, ...)
    if (verbose > 1) {
      message("Activity data now in R")
    }
    lux_path <- file.path(path, "lux.bin")
    if (file.exists(lux_path)) {
      stopifnot(info$`Serial Prefix` %in% c("NEO", "MRA"))
      if (info$`Serial Prefix` == "NEO") {
        lux_scale_factor = 1.25
        lux_max_value = 2500L
      }
      if (info$`Serial Prefix` == "MRA") {
        lux_scale_factor = 3.25
        lux_max_value = 6000L
      }
      luxdata <- parseLuxBin(
        lux_path, max_samples = samples,
        scale_factor = lux_scale_factor,
        max_value = lux_max_value,
        verbose = as.logical(verbose))
      attr(accdata, "light_data") = luxdata
    }

  }

  if (verbose > 1) {
    message("Adding attributes")
  }
  attr(accdata, "start_time") <- info[["Start Date"]]
  attr(accdata, "stop_time") <- info[["Stop Date"]]
  attr(accdata, "subject_name") <- info[["Subject Name"]]
  attr(accdata, "time_zone") <- info[["TimeZone"]]
  attr(accdata, "firmware") <- info[["Firmware"]]
  attr(accdata, "serial_prefix") <- info[["Serial Prefix"]]
  attr(accdata, "old_version") <- is_old_version
  if (!is_old_version) {
    attr(accdata, "missingness") <- data.frame(
      time = as.POSIXct(as.numeric(names(attr(accdata, "missingness"))),
                        origin = "1970-01-01", tz = tz),
      n_missing = attr(accdata, "missingness"),
      stringsAsFactors = FALSE)
  }

  if (verbose) {
    message("Done", " (in ",
            as.numeric(
              difftime(Sys.time(),
                       fun_start_time, units = "secs")),
            " seconds)"
    )
  }

  x <- structure(accdata,
                 class = c("activity", class(accdata)))

  if (asDataFrame)
    x <- as.data.frame(x)

  x

}

#' Convert an activity matrix to a data.frame
#'
#' @param x Object of class 'activity' (returned by read.gt3x)
#' @param ... not used
#' @param verbose print diagnostic messages
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
as.data.frame.activity <- function(x, ..., verbose = FALSE) {
  dig = getOption("digits")
  dig.sec = getOption("digits.secs")
  options(digits = 15, digits.secs = 3)
  on.exit({
    options(digits = dig, digits.secs = dig.sec)
  })
  all_attributes = attributes(x)

  tz <- "GMT" # used for parsing, timestamps are actually in local time
  start_time <- as.numeric(all_attributes[["start_time"]], tz = tz)
  time_index <- all_attributes[["time_index"]]
  sample_rate <- all_attributes[["sample_rate"]]

  if (verbose) {
    message("Converting to a data.frame ...")
  }
  class(x) = "matrix"
  attr(x, "time_index") = NULL
  # attr(x, "missingness") = NULL
  # x = as.data.frame(x)
  # x$time = start_time + time_index/sample_rate;
  x <- activityAsDataFrame(x, time_index, start_time, sample_rate)
  x$time <- as.POSIXct(x$time, origin = "1970-01-01", tz = tz)
  if (verbose) {
    message("Done")
  }
  x = structure(x,
            class = c("activity_df", class(x)),
            subject_name = all_attributes[["subject_name"]],
            time_zone = all_attributes[["time_zone"]],
            missingness = all_attributes[["missingness"]])
  attr(x, "light_data") = all_attributes[["light_data"]]
  attr(x, "old_version") = all_attributes[["old_version"]]
  attr(x, "firmware") <- all_attributes[["firmware"]]
  attr(x, "serial_prefix") <- all_attributes[["serial_prefix"]]
  attr(x, "old_version") <- all_attributes[["old_version"]]
  attr(x, "sample_rate") <- all_attributes[["sample_rate"]]

    x
}


#' Print the contents of the activity data
#'
#' @param x gt3x_info object returned by parse_gt3x_info()
#' @param ... additional arguments passed to \code{\link{head}}
#'
#' @export
#' @rdname print
print.activity_df <- function(x, ...) {
  cat(paste0("Sampling Rate: ", attr(x, "sample_rate"), "Hz\n"))
  cat(paste0("Firmware Version: ", attr(x, "firmware"), "\n"))
  cat(paste0("Serial Number Prefix: ", attr(x, "serial_prefix"), "\n"))
  class(x) = "data.frame"
  print(head(x, ...))
}


#' @rdname print
#' @export
head.activity_df = function(x, ...) {
  all_attr = attributes(x)
  nattr = names(all_attr)
  nattr = setdiff(nattr, c("dim", "dimnames", "names", "rownames"))
  class(x) = "data.frame"
  x = head(x, ...)
  for (iattr in nattr) {
    attr(x, iattr) =  all_attr[[iattr]]
  }
  class(x) = c("activity_df", class(x))
  x
}

#' @rdname print
#' @export
print.activity <- function(x, ...) {
  cat(paste0("Sampling Rate: ", attr(x, "sample_rate"), "Hz\n"))
  cat(paste0("Firmware Version: ", attr(x, "firmware"), "\n"))
  cat(paste0("Serial Number Prefix: ", attr(x, "serial_prefix"), "\n"))
  class(x) = "matrix"
  print(head(x, ...))
}

#' @rdname print
#' @importFrom utils head
#' @export
head.activity = function(x, ...) {
  all_attr = attributes(x)
  nattr = names(all_attr)
  nattr = setdiff(nattr, c("dim", "dimnames", "names", "rownames"))
  class(x) = "matrix"
  x = utils::head(x, ...)
  for (iattr in nattr) {
    attr(x, iattr) =  all_attr[[iattr]]
  }
  class(x) = c("activity", class(x))
  x
}
