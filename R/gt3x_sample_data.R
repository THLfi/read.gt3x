#' Path to read.gt3x package sample data
#'
<<<<<<< HEAD
#' @param index The index of a sample file to retrieve
#'
#' @examples
#' dir <- gt3x_datapath()
#'
#' @export
gt3x_datapath <- function(index = NULL) {
  datadir <- file.path(path.package("read.gt3x"), "extdata")
  if(!is.null(index)) {
    files <- list_gt3x(datadir)
    if(index > length(files)) stop("Index is larger than the number of sample files, which there are ", length(files))
=======
#' @param index Integer. The index of a sample file to retrieve. If NULL (default) the path to the directory
#' including the sample files will be returned.
#'
#' @examples
#' dir <- gt3x_datapath()
#' gt3x_filename <- gt3x_datapath(1)
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
      gt3x_download(url = gt3x_url(i), destfile = file.path(datadir, filenames[i]))
  }
  if(!is.null(index)) {
    files <- list_gt3x(datadir)
>>>>>>> github
    return(files[index])
  }
  datadir
}
<<<<<<< HEAD
=======

#' Download and unzip a zipped gt3xfile
gt3x_download <- function(url, destfile) {
  message("Downloading gt3x sample data...")
  temp <- tempfile()
  download.file(url, temp, method = "auto")
  unzip(temp, exdir = destfile)
  unlink(temp)
}

#' Get url of gt3x sample files
gt3x_url <- function(index = NULL, filename = NULL, version = "v0.2-alpha", baseurl = "https://github.com/THLfi/read.gt3x/releases/download") {
  dataurl <- gt3x_dataurl(version, baseurl)
  if(is.null(filename))
    filename <- gt3x_filename(index, zipped = TRUE)
  file.path(dataurl, filename)
}

#' Get url of github release
gt3x_dataurl <- function(version = "v0.2-alpha", baseurl = "https://github.com/THLfi/read.gt3x/releases/download") {
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
>>>>>>> github
