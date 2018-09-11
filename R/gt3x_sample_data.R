#' Path to read.gt3x package sample data
#'
#' @param index Integer. The index of a sample file to retrieve. If NULL (default) the path to the directory
#' including the sample files will be returned.
#'
#' @examples
#' dir <- gt3x_datapath()
#' gt3x_filename <- gt3x_datapath(1)
#'
#' @family file manipulations
#'
#' @export
gt3x_datapath <- function(index = NULL) {
  homedir <-  path.expand('~')
  datadir <- file.path(homedir, ".read.gt3x-data")
  if(!dir.exists(datadir))
    dir.create(datadir)
  filenames <- gt3x_filename(index)
  for(i in seq_along(filenames)) {
    if(!file.exists(file.path(datadir, filenames[i])))
      gt3x_download(url = gt3x_url(i), exdir = datadir)
  }
  if(!is.null(index)) {
    files <- list_gt3x(datadir)
    return(files[index])
  }
  datadir
}


#' Download and unzip a zipped gt3xfile
#'
#' @family sample-data
gt3x_download <- function(url, exdir) {
  message("Downloading gt3x sample data...")
  temp <- tempfile()
  download.file(url, temp, method = "auto")
  unzip(temp, exdir = exdir)
  unlink(temp)
}

#' Get url of gt3x sample file
#'
#' @family sample-data
gt3x_url <- function(index = NULL, filename = NULL) {
  dataurl <- gt3x_dataurl()
  if(is.null(filename))
    filename <- gt3x_filename(index, zipped = TRUE)
  file.path(dataurl, filename)
}

#' Get url of github release
#'
#' @family sample-data
gt3x_dataurl <- function(version = "v1.0", baseurl = "https://github.com/THLfi/read.gt3x/releases/download") {
  url <- file.path(baseurl, version)
}

#' Get sample gt3x filenames
gt3x_filename <- function(index = NULL, zipped = FALSE) {
  files <- c("EE_left_29.5.2017-05-30.gt3x", "SS_left_19.5.2017-05-22.gt3x")
  if(zipped)
    files <- paste0(files, ".zip")
  if(is.null(index)) return(files)
  if(index > length(files)) stop("Index is larger than the number of available files")
  if(index < 1) stop("Index can't be less than 1")
  files[index]
}

