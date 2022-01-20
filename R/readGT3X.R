#' @useDynLib read.gt3x
#' @importFrom Rcpp sourceCpp
NULL


#' Read GT3X
#'
#' Read activity samples from a GT3X file as a matrix.
#' Please note that all timestamps are in local time (of the device)
#' even though they are represented as \code{POSIXct} with GMT timezone.
#'
#' @param path Path to gt3x folder
#' @param asDataFrame convert to an `activity_df`, see
#' \code{as.data.frame.activity}
#' @param imputeZeroes Impute zeros in case there are missingness?
#' Default is FALSE, in which case
#' the time series will be incomplete in case there is missingness.
#' @param ... additional arguments to pass to \code{parseGT3X} C++ code, e.g. batch-loading options as now documented in vignette "Batch loading a gt3x file"
#' @param verbose print diagnostic messages
#' @param cleanup should any unzipped files be deleted?
#' @param add_light add light data to the `data.frame` if data exists in the
#' GT3X
#' @param flag_idle_sleep flag idle sleep mode.  If \code{imputeZeroes = TRUE},
#' this finds where all 3 axes are zero.
#'
#' @note
#'
#' The timestamps in the .gt3x data format are saved in .NET format, which is
#'  nanoseconds in local time since 0001-01-01.
#' This is a bit tricky to parse into an R datetime format. DateTimes are
#' therefore represented as \code{POSIXct} format with the
#' 'GMT' timezone attribute, which is false; the datetime actually
#' represents local time.
#'
#' @return A numeric matrix with 3 columns (X, Y, Z) and the following
#' attributes:
#'  \itemize{
#' \item \code{start_time} :  Start time from info file in \code{POSIXct} format.
#' \item \code{subject_name} : Subject name from info file
#' \item \code{time_zone} : Time zone from info file
#' \item \code{missingness} : Named integer vector. Names are \code{POSIXct}
#' timestamps and values are the number of missing values.
#' }
#'
#' @examples
#'
#' gt3xfile <-
#'   system.file(
#'     "extdata", "TAS1H30182785_2019-09-17.gt3x",
#'     package = "read.gt3x")
#' is_gt3x(gt3xfile)
#' have_log_and_info(gt3xfile, verbose = TRUE)
#'
#' x <- read.gt3x(gt3xfile, imputeZeroes = FALSE, asDataFrame = FALSE,
#' verbose = TRUE)
#' attr(x, "features")
#' df2 <- as.data.frame(x, verbose = TRUE)
#' attr(df2, "features")
#' head(df2)
#' rm(x); gc(); gc()
#' rm(df2); gc()
#'
#' x <- read.gt3x(gt3xfile, imputeZeroes = TRUE, asDataFrame = TRUE,
#' verbose = TRUE)
#'
#' \dontrun{
#' # first unzip, then read
#' datadir <- gt3x_datapath()
#' gt3xfolders <- unzip.gt3x(datadir)
#' gt3xfile <- gt3xfolders[2]
#' # temporary unzip, read, convert to a data frame
#' gt3xfile <- gt3x_datapath(1)
#' memory.limit()
#' df <- read.gt3x(gt3xfile, asDataFrame = FALSE, verbose = 2)
#' head(df)
#' rm(df); gc(); gc()
#'
#'
#' df <- read.gt3x(gt3xfile, asDataFrame = TRUE, verbose = 2)
#' head(df)
#' }
#'
#' \dontrun{
#'
#'
#'
#' url <- paste0("https://github.com/THLfi/read.gt3x/",
#' "files/", "3522749/", "GT3X%2B.01.day.gt3x.zip")
#' destfile <- tempfile(fileext = ".zip")
#' dl <- download.file(url, destfile = destfile, mode = "wb")
#' gt3x_file <- unzip(destfile, exdir = tempdir())
#' gt3x_file <- gt3x_file[!grepl("__MACOSX", gt3x_file)]
#' path <- gt3x_file
#'
#' res <- read.gt3x(path)
#'
#' gz <- R.utils::gzip(path, remove = FALSE, overwrite = FALSE)
#' df2 <- read.gt3x(gz, asDataFrame = FALSE, verbose = 2)
#' head(df2)
#'
#' rm(df2); gc(); gc()
#'
#' }
#' @family gt3x-parsers
#'
#' @export
read.gt3x <- function(path, verbose = FALSE, asDataFrame = FALSE,
                      imputeZeroes = FALSE,
                      flag_idle_sleep = FALSE,
                      cleanup = FALSE,
                      ...,
                      add_light = FALSE) {

  verbose_message <- function(..., verbose = verbose) {
    if (verbose) {
      message(...)
    }
  }
  fun_start_time <- Sys.time()

  path <- unzip_zipped_gt3x(path, cleanup = cleanup)
  remove_path <- path
  remove_file <- attr(path, "remove")
  if (is.null(remove_file)) {
    remove_file <- FALSE
  }

  has_info <- have_info(path)
  if (has_info) {
    info <- parse_gt3x_info(path)
  }
  files <- c("info.txt", "log.bin")
  if (has_info) {
    is_old_version <- old_version(info)
    if (is_old_version) {
      files <- c("info.txt",
                 "activity.bin",
                 "lux.bin")
    }
  }

  if (is_gt3x(path)) {
    verbose_message(
      paste0("Input is a .gt3x file, unzipping to a ",
             "temporary location first..."),
      verbose = verbose)
    path <- unzip.gt3x(path,
                       verbose = verbose,
                       files = files,
                       check_structure = !is_old_version,
                       location = tempdir())
    on.exit(unlink(path, recursive = TRUE))
  }

  tz  <- "GMT" # used for parsing, times are actually in local timezone
  info <- parse_gt3x_info(path, tz = tz)

  if (verbose) {
    print(info)
  }

  samples <- get_n_samples(info)
  bad_samples <- attr(samples, "bad")

  verbose_message(
    "Parsing GT3X data via CPP.. expected sample size: ", samples,
    verbose = verbose
  )
  xyz = c("X", "Y", "Z")
  if (!is_old_version) {
    logpath <- file.path(path, "log.bin")
    stopifnot(length(info$`Acceleration Scale`) > 0)
    stopifnot(length(info$`Sample Rate`) > 0)
    stopifnot(length(info$`Start Date`) > 0)
    accdata <- parseGT3X(
      logpath, max_samples = samples,
      scale_factor = info[["Acceleration Scale"]],
      sample_rate = info[["Sample Rate"]],
      start_time = as.numeric(info[["Start Date"]]),
      verbose = as.logical(verbose),
      impute_zeroes = imputeZeroes, ...)
    # need reordering for Y X Z ACTIVITY PACKETS
    tmp_at <- attributes(accdata)
    accdata <- accdata[, xyz]
    tmp_at$dim <- dim(accdata)
    tmp_at$dimnames[[2]] <- xyz
    tmp_at$features <- get_features(tmp_at$features)
    attributes(accdata) <- tmp_at
    rm(tmp_at)

    if (verbose > 1) {
      message("Activity data now in R")
    }
  } else {
    verbose_message("Using NHANES-GT3X format - older format",
                    verbose = verbose)
    act_path <- file.path(path, "activity.bin")
    est_n_samples <- floor(file.size(act_path) * 8 / 36)
    if (est_n_samples > samples) {
      warning(
        paste0("Estimated samples is larger than from get_n_samples",
               " from time data, using estimated size.  If this errors,",
               " please file an issue."))
      samples = est_n_samples
    }
    accdata <- parseActivityBin(
      act_path, max_samples = samples,
      scale_factor = info$`Acceleration Scale`,
      sample_rate = info$`Sample Rate`,
      verbose = as.logical(verbose),
      ...)
    tmp_at = attributes(accdata)
    index = min(est_n_samples, nrow(accdata))
    accdata = accdata[seq(index),]
    tmp_at$time_index = tmp_at$time_index[seq(index)]
    accdata = accdata[, xyz]
    tmp_at$dim = dim(accdata)
    tmp_at$dimnames[[2]] = xyz
    attributes(accdata) = tmp_at
    rm(tmp_at)
    verbose_message("Activity data now in R", verbose = verbose > 1)

    lux_path <- file.path(path, "lux.bin")
    luxdata <- parse_lux_data(lux_path, info = info,
                              samples = samples, verbose = verbose > 1)
    attr(accdata, "light_data") <- luxdata[seq(est_n_samples)]
  }
  gc()
  attr(accdata, "add_light") = add_light
  rdata = range(c(accdata[, xyz]))
  #!!! Need ISSUE/check
  if (any(abs(rdata) > 20)) {
    warning("Data seems too large (> 20) - checksum may be wrong - check data")
  }
  #!!! Need ISSUE/check
  check = anyDuplicated(attr(accdata, "time_index")) > 0
  if (check) {
    warning("Duplicated time indices - usually indicative of a problem!")
  }

  if (cleanup) {
    if (remove_file) {
      file.remove(remove_path)
    }
    rm_files <- file.path(path,
                          c("log.bin", "info.txt", "activity.bin",
                            "lux.bin"))
    suppressWarnings({
      file.remove(rm_files)
    })
  }

  verbose_message("Adding attributes", verbose = verbose > 1)

  attr(accdata, "start_time") <- info[["Start Date"]]
  attr(accdata, "stop_time") <- info[["Stop Date"]]
  attr(accdata, "last_sample_time") <- info[["Last Sample Time"]]
  attr(accdata, "subject_name") <- info[["Subject Name"]]
  attr(accdata, "time_zone") <- info[["TimeZone"]]
  attr(accdata, "firmware") <- info[["Firmware"]]
  attr(accdata, "serial_prefix") <- info[["Serial Prefix"]]
  attr(accdata, "acceleration_min") <- info[["Acceleration Min"]]
  attr(accdata, "acceleration_max") <- info[["Acceleration Max"]]

  attr(accdata, "bad_samples") <- bad_samples
  attr(accdata, "old_version") <- is_old_version

  attr(accdata, "header") <- info
  if (!is_old_version) {
    attr(accdata, "missingness") <- data.frame(
      time = as.POSIXct(as.numeric(names(attr(accdata, "missingness"))),
                        origin = "1970-01-01", tz = tz),
      n_missing = attr(accdata, "missingness"),
      stringsAsFactors = FALSE)
  }

  verbose_message(
    "Done", " (in ",
    as.numeric(
      difftime(Sys.time(),
               fun_start_time, units = "secs")),
    " seconds)",
    verbose = verbose)


  accdata <- structure(accdata,
                       class = c("activity", class(accdata)))

  if (asDataFrame)
    accdata <- as.data.frame(accdata, verbose = verbose > 1)

  if (flag_idle_sleep) {
    if (asDataFrame) {
      accdata$idle = rowSums(accdata[, c("X", "Y", "Z")] == 0) == 3
    } else {
      accdata = cbind(accdata,
                      idle = rowSums(accdata[, c("X", "Y", "Z")] == 0) == 3)
    }
  }

  accdata

}

#' Convert an activity matrix to a data.frame
#'
#' @param x Object of class 'activity' (returned by read.gt3x)
#' @param ... not used
#' @param verbose print diagnostic messages
#' @param add_light add light data to the `data.frame` if data exists in the
#' GT3X
#'
#' @family gt3x-parsers
#'
#' @return An object of class `activity_df` which is also a data.frame with
#' the following attributes (and more)
#' \itemize{
#' \item \code{subject_name} : Subject name from info file
#' \item \code{time_zone} : Time zone from info file
#' \item \code{missingness} : Data frame with timestamps and the number of missing values associated.
#' }
#'
#' @export
as.data.frame.activity <- function(x, ..., verbose = FALSE,
                                   add_light = FALSE) {

  all_attributes <- attributes(x)
  attr(x, "time_index") <- NULL

  if (missing(add_light)) {
    add_light = attr(x, "add_light")
  }

  tz <- "GMT" # used for parsing, timestamps are actually in local time
  start_time <- as.numeric(all_attributes[["start_time"]], tz = tz)
  time_index <- all_attributes[["time_index"]]
  stopifnot(!is.null(time_index))
  all_attributes[["time_index"]] = NULL
  sample_rate <- all_attributes[["sample_rate"]]

  if (verbose) {
    message("Converting to a data.frame ...")
  }

  if (verbose) {
    if (time_index[1] != 0) {
      message(paste0("First time index is: ", time_index[1]))
    }
  }
  divider = 100L


  # datetime parsing currently different for old and new formats
  # divider <- if (all_attributes[["old_version"]]) sample_rate else 100
  class(x) <- "matrix"
  # x <- activityAsDataFrame(x, time_index, start_time, divider)
  x = as.data.frame(x)
  x$time = start_time + time_index/divider;
  x = x[, c("time", setdiff(colnames(x), "time"))]
  if ("idle" %in% colnames(x)) {
    x$idle = x$idle > 0
  }

  x$time <- as.POSIXct(x$time, origin = "1970-01-01", tz = tz)
  if (add_light) {
    x$lux = all_attributes$light_data
    all_attributes$light_data = NULL
  }
  if (verbose) {
    missingness <- all_attributes$missingness
    if (!all(as.numeric(missingness$time) %% 1 == 0)) {
      missingness$time <- round(missingness$time)
    }
    dt <- difftime(x$time[1], all_attributes[["start_time"]], units = "secs")
    dt <- abs(as.numeric(dt))
    if (dt > 1 && !(round(x$time[1]) %in% missingness$time)) {
      warning("Start time does not match header start time")
    }
    rm(missingness)
    rm(dt)
  }
  if (verbose) {
    message("Done")
  }
  x <- structure(x,
                 class = c("activity_df", class(x)),
                 subject_name = all_attributes[["subject_name"]],
                 time_zone = all_attributes[["time_zone"]],
                 missingness = all_attributes[["missingness"]])
  attr(x, "light_data") <- all_attributes[["light_data"]]
  attr(x, "old_version") <- all_attributes[["old_version"]]
  attr(x, "firmware") <- all_attributes[["firmware"]]
  attr(x, "last_sample_time") <- all_attributes[["last_sample_time"]]
  attr(x, "serial_prefix") <- all_attributes[["serial_prefix"]]
  attr(x, "old_version") <- all_attributes[["old_version"]]
  attr(x, "sample_rate") <- all_attributes[["sample_rate"]]
  attr(x, "acceleration_min") <- all_attributes[["acceleration_min"]]
  attr(x, "acceleration_max") <- all_attributes[["acceleration_max"]]
  attr(x, "header") <- all_attributes[["header"]]
  attr(x, "start_time") <- all_attributes[["start_time"]]
  attr(x, "stop_time") <- all_attributes[["stop_time"]]
  attr(x, "total_records") <- all_attributes[["total_records"]]
  attr(x, "bad_samples") <- all_attributes[["bad_samples"]]
  attr(x, "features") <- all_attributes[["features"]]

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

  old <- options()         # code line i
  on.exit(options(old))    # code line i+1
  options(digits = 15L, digits.secs = 3L)

  arglist <- list(...)
  if(! "n" %in% names(arglist)) {
    if(!is.null(attr(x, "n_head")))
      arglist$n <- attr(x, "n_head")
  }
  cat(paste0("Sampling Rate: ", attr(x, "sample_rate"), "Hz\n"))
  cat(paste0("Firmware Version: ", attr(x, "firmware"), "\n"))
  cat(paste0("Serial Number Prefix: ", attr(x, "serial_prefix"), "\n"))
  class(x) <- "data.frame"
  print(do.call(head, c(list(x), arglist)))
}


#' @rdname print
#' @export
head.activity_df <- function(x, ...) {
  all_attr <- attributes(x)
  nattr <- names(all_attr)
  nattr <- setdiff(nattr, c("dim", "dimnames", "names", "rownames"))
  class(x) <- "data.frame"
  x <- head(x, ...)
  for (iattr in nattr) {
    attr(x, iattr) <-  all_attr[[iattr]]
  }
  class(x) <- c("activity_df", class(x))

  attr(x, "n_head") <- get_n_head(...)
  x
}

#' @rdname print
#' @export
print.activity <- function(x, ...) {

  arglist <- list(...)
  if(! "n" %in% names(arglist)) {
    if(!is.null(attr(x, "n_head")))
      arglist$n <- attr(x, "n_head")
  }

  cat(paste0("Sampling Rate: ", attr(x, "sample_rate"), "Hz\n"))
  cat(paste0("Firmware Version: ", attr(x, "firmware"), "\n"))
  cat(paste0("Serial Number Prefix: ", attr(x, "serial_prefix"), "\n"))
  class(x) <- "matrix"
  print(do.call(head, c(list(x), arglist)))
}

#' @rdname print
#' @importFrom utils head
#' @export
head.activity <- function(x, ...) {
  all_attr <- attributes(x)
  nattr <- names(all_attr)
  nattr <- setdiff(nattr, c("dim", "dimnames", "names", "rownames"))
  class(x) <- "matrix"
  x <- utils::head(x, ...)
  for (iattr in nattr) {
    attr(x, iattr) <-  all_attr[[iattr]]
  }
  class(x) <- c("activity", class(x))

  attr(x, "n_head") <- get_n_head(...)

  x
}

get_n_head <- function(...) {
  arglist <- list(...)
  n_head <- NULL
  if("n" %in% names(arglist)) {
    n_head<- arglist[["n"]]
  } else if(is.null(names(arglist)) & length(arglist) == 1) {
    n_head<- arglist[[1]]
  }
  n_head
}

parse_lux_data <- function(lux_path, info, samples, verbose = TRUE) {
  luxdata <- NULL
  if (file.exists(lux_path)) {
    stopifnot(info$`Serial Prefix` %in% c("NEO", "MRA"))
    if (info$`Serial Prefix` == "NEO") {
      lux_scale_factor <- 1.25
      lux_max_value <- 2500L
    }
    if (info$`Serial Prefix` == "MRA") {
      lux_scale_factor <- 3.25
      lux_max_value <- 6000L
    }
    luxdata <- parseLuxBin(
      lux_path, max_samples = samples,
      scale_factor = lux_scale_factor,
      max_value = lux_max_value,
      verbose = as.logical(verbose))
  }
  luxdata
}


get_features = function(features) {
  if (is.null(features)) {
    return(NULL)
  }
  feat <- c("heart rate monitor", "data summary", "sleep mode", "proximity tagging",
            "epoch data", "no raw data")
  features <- as.integer(intToBits(features))[1:5] > 0
  if (!any(features)) {
    features <- "none"
  } else {
    features <- paste(feat[features], collapse = ",")
  }
  features
}
