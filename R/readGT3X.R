#' @useDynLib accelerometer
#' @importFrom Rcpp sourceCpp
NULL


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
is_gt3x <- function(path) sapply(path, function(f) grepl("\\.gt3x$", f))

#' List all gt3x files in a directory
#'
#' @export
list_gt3x <- function(dirpath) {
  files <- list.files(path = dirpath, full.names = TRUE)
  gt3xfiles <- files[is_gt3x(files)]
  gt3xfiles
}

#' Check if a .gt3x file or unzipped gt3x directory has both log.bin adn info.txt
have_log_and_info <- function(gt3x) {
  if(is_gt3x(gt3x))
    filenames <- unzip(gt3x, list = TRUE)$Name
  else
    filenames <- list.files(gt3x)
  haslog <- "log.bin" %in% filenames
  hasinfo <- "info.txt" %in% filenames
  if(!haslog) message(gt3x, " doesn't contain log.bin")
  if(!hasinfo) message(gt3x, " doesn't contain info.txt")
  return(haslog & hasinfo)
}

#' Unzip a gt3x file
#'
#' @param path Path to the gt3xfile
#' @param dirname The name of the result directory. Default is the name of the input, sans the .gt3x extension.
#' @param location A path to a directory to unzip the folders to.
#' @param files The files to extract. Default is info.txt and log.bin
#'
#' @details
#'
#' A .gt3x file is a zipped directory with two files: log.bin and info.txt.
#' This function unzips the contents of the directory.
unzip.gt3x <- function(path, dirname =  basename(gsub(".gt3x$| ","", path)), location = tempdir(), files = c("info.txt", "log.bin")) {

  cat("Unzipping ", path, "\n")

  if(!is_gt3x(path)) {
    message(path, " is a not a .gt3x file. Unzipping failed")
    return(NULL)
  }

  if(!have_log_and_info(path)) {
    message(path, " did not contain both log.bin and info.txt. Unzipping failed.")
    return(NULL)
  }

  exdir <- file.path(location, dirname)
  extractedpaths <- unzip(path, files = files, exdir = exdir)
  cat(" === info.txt and log.bin extracted to ", exdir, "\n")
  exdir
}

#' Unzip gt3x files
#'
#' This is a wrapper for unzip.gt3x. unzipGT3X makes it more convenient to unzip multiple .gt3x files.
#'
#' @param path One of the following: (1) A path to a directory with .gt3x files in which case they are all unzipped, or
#' (2) A character vector of direct paths to .gt3x files.
#' @param location Path to a directory to unzip the files to. Default is a temporary directory created by tempdir().
#'
#' @details
#'  A .gt3x file is a zipped directory with two files: log.bin and info.txt.
#'  This function simply unzips the contents of the directories.
#'
#' @return
#' Returns a vector of paths to unzipped gt3x folders.
#'
#' @examples
#'
#' @export
unzipGT3X <- function(path, location = tempdir()) {
  if(length(path) == 1 & !is_gt3x(path[1])) {
    gt3xfiles <- list_gt3x(path)
  } else {
    gt3xfiles <- path
    if(!all(is_gt3x(gt3xfiles))) stop("Some or all of the filepaths do not have a .gt3x extension")
  }

  n <- length(gt3xfiles)
  if(n < 1) stop("No .gt3x files found")

  message("Unzipping gt3x data to ", location)

  result_paths <- vector("character", n)
  for(i in seq_len(n)) {
    cat(i, "/", n, " ", sep = "")
    result_paths[i] <- unzip.gt3x(gt3xfiles[i], location = location)
  }
  result_paths
}

#' Convert NET ticks to POSIXct datetime
#'
#' ref: # https://stackoverflow.com/questions/35240874/r-net-ticks-to-timestamp-in-r
ticks2datetime <- function(ticks, tz) {
  ticks <- as.numeric(ticks)
  seconds <- ticks / 1e7
  datetime <- as.POSIXct(seconds, origin='0001-01-01', tz=tz)
  datetime
}

#' Parse GT3X info.txt file
#'
#' @param path Path to a .gt3x file or an unzipped gt3x directory
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
print.gt3x_info <- function(x) {
  cat("GT3X information\n")
  str(x, give.head = FALSE, no.list=TRUE)
}

#' Calculate the expected activity sample size from start time and last sample time in the info.txt of a gt3x directory
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
#' gt3xfolders <- unzipGT3X(datadir)
#' x <- readGT3X(gt3xfolders[1])
#'
#' # temporary unzip and read
#' gt3xfile <- gt3x_datapath(1)
#' x <- readGT3X(gt3xfile)
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

  colnames(accdata) <- c( "X", "Y", "Z" , "timestamp")
  attr(accdata, "start_time") = as.POSIXct(attr(accdata, "start_time"), origin = "1970-01-01")

  message("Done", " (in ",  as.integer(Sys.time() - fun_start_time), " seconds)")

  structure(accdata,
            sample_rate = info$`Sample Rate`,
            class = c("activity", class(accdata)))

}

#' Path to accelerometer package sample data
#'
#' @param index The index of a sample file to retrieve
#'
#' @export
gt3x_datapath <- function(index = NULL) {
  datadir <- file.path(path.package("accelerometer"), "extdata")
  if(!is.null(index)) {
    files <- list_gt3x(datadir)
    if(index > length(files)) stop("Index is larger than the number of sample files, which there are ", length(files))
    return(files[index])
  }
  datadir
}

