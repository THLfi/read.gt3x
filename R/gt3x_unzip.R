
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
#' @param remove_original Remove the zipfile after unzipping?
#'
unzip_single_gt3x <- function(path, dirname =  basename(gsub(".gt3x$| ","", path)), location = tempdir(), files = c("info.txt", "log.bin"), remove_original = FALSE) {

  message("Unzipping ", path)

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
  stopifnot(length(extractedpaths) > 0)
  message(" === info.txt and log.bin extracted to ", exdir)
  if(remove_original) {
    message("Removing original zipfile...")
    removed <- file.remove(path)
    if(removed) message("Removed ", path)
  }
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
#' @param remove_original Remove the zipfiles after unzipping?
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
#' # unzip a single .gt3x file
#' path <- gt3x_datapath(1)
#' gt3xdir <- unzip.gt3x(path)
#'
#' # unzip multiple .gt3x files
#' dir <- gt3x_datapath()
#' gt3xdirs <- unzip.gt3x(dir)
#'
#' @export
unzip.gt3x <- function(path, location = tempdir(), remove_original = FALSE) {
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
    message(i, "/", n, " ", sep = "")
    result_paths[i] <- unzip_single_gt3x(gt3xfiles[i], location = location, remove_original = remove_original)
  }
  result_paths
}
