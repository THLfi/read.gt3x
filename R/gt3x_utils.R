
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
#' @examples
#'
#' is_gt3x("test.gt3x") # TRUE
#' is_gt3x("test") # FALSE
#'
is_gt3x <- function(path) {
  if(length(path) == 0) return(F)
  sapply(path, function(f) grepl("\\.gt3x$", f))
}

#' List full paths to all gt3x files in a directory
#'
#' @examples
#' list_gt3x(gt3x_datapath())
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

#' Unzip a single gt3x file
#'
#' A .gt3x file is a zipped archive with two files: log.bin and info.txt.
#' This function unzips the contents of the archive to a single folder.
#' This is a helper for unzip.gt3x()
#'
#' @param path Path to a .gt3x file
#' @param dirname The name of the resulting directory where the content of <path> are extracted.
#' Default is the name of the input file, sans the .gt3x extension.
#' @param location A path to an output directory. Default is a tempdir().
#' @param files The names of files to extract. Default is info.txt and log.bin
#'
unzip_single_gt3x <- function(path, dirname =  basename(gsub(".gt3x$| ","", path)), location = tempdir(), files = c("info.txt", "log.bin")) {

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
#' unzip.gt3x() makes it convenient to unzip multiple .gt3x files.
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
#' # unzip a single .gt3x file
#' path <- gt3x_datapath(1)
#' gt3xdir <- unzip.gt3x(path)
#'
#' # unzip multiple .gt3x files
#' dir <- gt3x_datapath()
#' gt3xdirs <- unzip.gt3x(dir)
#'
#' @export
unzip.gt3x <- function(path, location = tempdir()) {
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
    result_paths[i] <- unzip_single_gt3x(gt3xfiles[i], location = location)
  }
  result_paths
}
